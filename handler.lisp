#|
 This file is a part of deeds
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.deeds)

(defvar *standard-event-loop*)

(defgeneric start (event-delivery))
(defgeneric stop (event-delivery))
(defgeneric issue (event event-delivery))
(defgeneric handle (event event-delivery))
(defgeneric handler (handler event-loop))
(defgeneric register-handler (handler event-loop))
(defgeneric deregister-handler (handler event-loop))
(defgeneric sort-handlers (handlers event-loop))
(defgeneric ensure-handlers-sorted (event-loop))
(defgeneric build-event-loop (handlers event-loop))
(defgeneric recompile-event-loop (event-loop))

(defclass event-delivery () ())
(defclass queued-event-delivery (event-delivery simple-tasks:queued-runner) ())
(defclass event-loop (queued-event-delivery) ())
(defclass handler (event-delivery) ())
(defclass parallel-handler (handler) ())
(defclass queued-handler (handler queued-event-delivery) ())
(defclass locally-blocking-handler (handler) ())
(defclass globally-blocking-handler (handler queued-event-delivery) ())
(defclass event-task (simple-tasks:task) ())
(defclass blocking-event-task (event-task) ())

(define-condition event-loop-condition ()
  ((event-loop :initarg :event-loop :accessor event-loop-condition-event-loop)))

(define-condition event-loop-handler-dependency-cycle-error (error event-loop-condition)
  ((handler :initarg :handler :accessor event-loop-condition-handler))
  (:report (lambda (c s) (format s "Dependency cycle for handler ~a in ~a."
                                 (event-loop-condition-event-loop c)
                                 (event-loop-condition-handler c)))))

(define-condition handler-condition ()
  ((handler :initarg :handler :accessor handler-condition-handler)))

(define-condition handler-thread-stop-failed-warning (warning handler-condition)
  ((thread :initarg :thread :accessor handler-condition-thread))
  (:report (lambda (c s) (format s "Thread ~a of handler ~a did not stop."
                                 (handler-condition-thread c)
                                 (handler-condition-handler c)))))

(defclass event-delivery ()
  ((delivery-function :initarg :delivery-function :accessor delivery-function))
  (:default-initargs
   :delivery-function #'print))

(defmethod start ((event-delivery event-delivery))
  event-delivery)

(defmethod stop ((event-delivery event-delivery))
  event-delivery)

(defmethod issue ((event event) (event-delivery event-delivery))
  (handle event event-delivery)
  event)

(defmethod handle ((event event) (event-delivery event-delivery))
  (funcall (delivery-function event-delivery) event))

(defclass queued-event-delivery (event-delivery simple-tasks:queued-runner)
  ())

(defmethod start ((event-delivery queued-event-delivery))
  (when (simple-tasks:status= event-delivery :runing)
    (cerror "Start anyway" "~a is already started!" event-delivery))
  (simple-tasks:make-runner-thread event-delivery)
  event-delivery)

(defmethod stop ((event-delivery queued-event-delivery))
  (simple-tasks:stop-runner event-delivery)
  event-delivery)

(defmethod issue ((event event) (event-delivery queued-event-delivery))
  (simple-tasks:schedule-task
   (make-instance 'event-task :event event) event-delivery)
  event)

(defclass event-loop (queued-event-delivery)
  ((handlers :initform (make-hash-table :test 'eql) :accessor handlers)
   (sorted-handlers :initform () :accessor sorted-handlers)
   (lock :initform (bt:make-recursive-lock "Event loop lock") :accessor lock)))

(defmethod handler ((name symbol) (event-loop event-loop))
  (gethash name (handlers event-loop)))

(defmethod handler ((handler handler) (event-loop event-loop))
  (if (name handler)
      (handler (name handler) event-loop)
      (gethash handler (handlers event-loop))))

(defmethod register-handler ((handler handler) (event-loop event-loop))
  ;; Secure against race conditions
  (let ((old (handler handler event-loop))
        (handlers (copy-hash-table (handlers event-loop))))
    (setf (gethash (or (name handler) handler) handlers) handler)
    (let* ((sorted-handlers (sort-handlers handlers event-loop))
           (loop-definition (build-event-loop sorted-handlers event-loop))
           (compiled-loop (compile-lambda loop-definition)))
      (bt:with-recursive-lock-held ((lock event-loop))
        (setf (delivery-function event-loop) compiled-loop)
        (setf (sorted-handlers event-loop) sorted-handlers)
        (setf (handlers event-loop) handlers)))
    (values handler old)))

(defmethod deregister-handler ((handler handler) (event-loop event-loop))
  ;; Secure against race conditions
  (remhash (or (name handler) handler) (handlers event-loop))
  (bt:with-recursive-lock-held ((lock event-loop))
    (setf (sorted-handlers event-loop) (remove handler (sorted-handlers event-loop)))
    (recompile-event-loop event-loop))
  handler)

(defmethod ensure-handlers-sorted ((event-loop event-loop))
  (setf (sorted-handlers event-loop)
        (sort-handlers (handlers event-loop) event-loop)))

(defmethod sort-handlers ((handlers hash-table) event-loop)
  (sort-handlers (loop for v being the hash-values of handlers collect v)
                 event-loop))

(defmethod sort-handlers ((handlers list) (event-loop event-loop))
  ;; Graph time, yeah! We want to do a topological sort here.
  (let ((edges (make-hash-table :test 'eql))
        (nodes (make-hash-table :test 'eql))
        (sorted ()))
    ;; Build the graph
    (push :end (gethash :start edges))
    (setf (gethash :start nodes) :unvisited)
    (setf (gethash :end nodes) :unvisited)
    (dolist (handler handlers)
      (setf (gethash handler nodes) :unvisited)
      (push handler (gethash :start edges))
      (push :end (gethash handler edges))
      (dolist (after (after handler))
        (push handler (gethash (handler after event-loop) edges)))
      (dolist (before (before handler))
        (push (handler before event-loop) (gethash handler edges))))
    ;; Sort it using Tarjan's algorithm
    (labels ((visit (node)
               (case (gethash node nodes)
                 (:temporary
                  (error 'event-loop-handler-dependency-cycle-error
                         :handler node :event-loop event-loop))
                 (:unvisited
                  (setf (gethash node nodes) :temporary)
                  (dolist (target (gethash node edges))
                    (visit target))
                  (remhash node nodes)
                  (when (typep node 'handler)
                    (push node sorted))))))
      (loop while (with-hash-table-iterator (iterator nodes)
                    (multiple-value-bind (found node) (iterator)
                      (when found (visit node) T)))))
    sorted))

(defun filter-tests (filter)
  (let ((tests ()))
    (labels ((r (part)
               (when (consp part)
                 (case (first part)
                   ((and or) (dolist (part (rest part))
                               (r part)))
                   (not (r (second part)))
                   (T (push part tests))))))
      (r filter))
    tests))

(defun replace-tests (filter type testmap)
  (labels ((r (part)
             (if (listp part)
                 (case (first part)
                   ((and or) `(,(first part) ,@(mapcar #'r (rest part))))
                   (not `(not ,(r (second part))))
                   (T (multiple-value-bind (test pure) (compile-test part type)
                        (if pure
                            (gethash test testmap part)
                            test))))
                 part)))
    (r filter)))

(defun test-gensym (test)
  (gensym (string (first test))))

(defun compile-test (test type)
  (let ((pure T))
    (values `(,(first test)
              ,@(loop for arg in (rest test)
                      collect (typecase arg
                                ((and symbol (not keyword))
                                 (multiple-value-bind (accessor slot) (build-fuzzy-slot-accessor arg type 'ev)
                                   (when (and (typep slot 'event-slot) (event-slot-mutable slot))
                                     (setf pure NIL))
                                   accessor))
                                (T arg))))
            pure)))

(defun extract-tests (handlers)
  ;; Really primitive, could be optimised a lot!
  ;; 
  ;; The initialisation of the tests is currently handled in a
  ;; flat manner, however since there's a hierarchy in the
  ;; events this should be reflected in the initialisation too
  ;; and thus could avoid a lot of superfluous testing.
  ;; 
  ;; Additionally currently some tests can coincide under
  ;; different type tests and would thus potentially get
  ;; repeatedly done unnecessarily.
  ;;
  ;; Finally, some combinations could be optimised by being
  ;; put into groups, rather than being done all for every
  ;; handler, however that would require a completely
  ;; different model of compiling these handlers.
  (let ((testmap (make-hash-table :test 'equal))
        (typemap (make-hash-table :test 'equal)))
    (flet ((insert-test (test)
             (setf (gethash test testmap) (test-gensym test))))
      (dolist (handler handlers)
        (let ((typetest `(typep ev ',(event-type handler))))
          (insert-test typetest)
          (dolist (test (filter-tests (filter handler)))
            (multiple-value-bind (test pure) (compile-test test (event-type handler))
              (when pure ;; If this is a test that includes a mutable field, we cannot cache it!
                (insert-test test)
                (pushnew test (gethash typetest typemap) :test 'equal)))))))
    (values
     (loop for test being the hash-keys of testmap
           for symbol being the hash-values of testmap
           collect (if (eql (first test) 'typep)
                       `(,symbol ,test)
                       symbol))
     (loop for type being the hash-keys of typemap
           for tests being the hash-values of typemap
           collect `(when ,(gethash type testmap)
                      ,@(loop for test in tests
                              collect `(setf ,(gethash test testmap) ,test))))
     (loop for handler in handlers
           collect `(and ,(gethash `(typep ev ',(event-type handler)) testmap)
                         ,(or (replace-tests (filter handler) (event-type handler) testmap) T))))))

(defmethod recompile-event-loop ((event-loop event-loop))
  (let* ((loop-definition (build-event-loop (sorted-handlers event-loop) event-loop))
         (compiled-loop (compile-lambda loop-definition)))
    (bt:with-recursive-lock-held ((lock event-loop))
      (setf (delivery-function event-loop) compiled-loop)))
  event-loop)

(defmethod build-event-loop ((handlers list) (event-loop event-loop))
  ;; Oh boy!
  (multiple-value-bind (cache inits filters) (extract-tests handlers)
    `(lambda (ev)
       (declare (optimize speed))
       (let ,cache
         (declare (dynamic-extent ,@(loop for var in cache
                                          collect (if (listp var) (car var) var))))
         ,@inits
         ,@(loop for filter in filters
                 for handler in handlers
                 collect `(when ,filter
                            (issue ev ,handler)))))))

(defmethod handle :around ((event event) (event-loop event-loop))
  (bt:with-recursive-lock-held ((lock event-loop))
    (call-next-method)))

(defmethod issue :before ((event event) (event-loop event-loop))
  (setf (issue-time event) (get-universal-time)))

(defclass handler (event-delivery)
  ((name :initarg :name :reader name)
   (event-type :initarg :event-type :reader event-type)
   (filter :initarg :filter :reader filter)
   (before :initarg :before :reader before)
   (after :initarg :after :reader after))
  (:default-initargs
   :name NIL
   :event-type 'event
   :filter NIL
   :before ()
   :after ()))

(defmethod print-object ((handler handler) stream)
  (print-unreadable-object (handler stream :type T :identity T)
    (format stream "~:[~;~s ~s~] ~s" (name handler) :name (name handler) (simple-tasks:status handler))))

(defclass parallel-handler (handler)
  ((threads :initform () :accessor threads)
   (lock :initform (bt:make-recursive-lock "parallel-handler lock") :accessor lock)))

(defmethod issue ((event event) (parallel-handler parallel-handler))
  (bt:with-lock-held ((lock parallel-handler))
    (let (thread)
      (setf thread (bt:make-thread (lambda ()
                                     (unwind-protect
                                          (with-simple-restart (abort "Stop the handler thread.")
                                            (handle event parallel-handler))
                                       (bt:with-lock-held ((lock parallel-handler))
                                         (setf (threads parallel-handler)
                                               (remove thread (threads parallel-handler))))))
                                   :name (format NIL "~a thread" parallel-handler)))
      (push thread (threads parallel-handler)))))

(defmethod stop ((parallel-handler parallel-handler))
  (loop for thread = (bt:with-lock-held ((lock parallel-handler))
                       (pop (threads parallel-handler)))
        while thread
        do (bt:interrupt-thread thread (lambda () (abort)))
           (loop repeat 100
                 do (unless (bt:thread-alive-p thread) (return))
                    (sleep 0.01)
                 finally (warn 'handler-thread-stop-failed-warning
                               :thread thread :handler parallel-handler))))

(defclass queued-handler (handler queued-event-delivery)
  ())

(defclass locally-blocking-handler (handler)
  ())

(defclass globally-blocking-handler (handler queued-event-delivery)
  ())

(defmethod issue ((event event) (globally-blocking-handler globally-blocking-handler))
  (simple-tasks:schedule-task
   (make-instance 'blocking-event-task :event event) globally-blocking-handler)
  event)

(defclass event-task (simple-tasks:task)
  ((event :initarg :event :accessor event-task-event)))

(defmethod simple-tasks:run-task ((event-task event-task))
  (handle (event-task-event event-task) (simple-tasks:runner event-task)))

(defclass blocking-event-task (event-task simple-tasks:blocking-task)
  ())
