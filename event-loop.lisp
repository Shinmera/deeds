#|
 This file is a part of deeds
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.deeds)

(defvar *standard-event-loop*) ;; See bottom

(defgeneric handler (handler event-loop))
(defgeneric (setf handler) (handler event-loop))
(defgeneric register-handler (handler event-loop))
(defgeneric deregister-handler (handler event-loop))
(defgeneric deliver-event-directly (event event-loop))
(defgeneric sort-handlers (handlers sorted-event-loop))
(defgeneric ensure-handlers-sorted (sorted-event-loop))
(defgeneric build-event-loop (handlers compiled-event-loop))
(defgeneric recompile-event-loop (compiled-event-loop))

(defclass event-loop (queued-event-delivery)
  ((handlers :initform (make-hash-table :test 'eql) :accessor handlers)
   (event-loop-lock :initform (bt:make-recursive-lock "Event loop lock") :accessor event-loop-lock))
  (:default-initargs
   :delivery-function NIL))

(defmethod handle ((event event) (event-loop event-loop))
  (if (delivery-function event-loop)
      (call-next-method)
      (deliver-event-directly event event-loop)))

(defmethod handler ((name symbol) (event-loop event-loop))
  (gethash name (handlers event-loop)))

(defmethod handler ((handler handler) (event-loop event-loop))
  (if (name handler)
      (handler (name handler) event-loop)
      (gethash handler (handlers event-loop))))

(defmethod (setf handler) ((handler handler) (event-loop event-loop))
  (setf (gethash (or (name handler) handler) (handlers event-loop)) handler))

(defmethod register-handler :around ((handler handler) (event-loop event-loop))
  (bt:with-recursive-lock-held ((event-loop-lock event-loop))
    (call-next-method)))

(defmethod register-handler ((handler handler) (event-loop event-loop))
  (let ((old (handler handler event-loop)))
    (setf (gethash (or (name handler) handler) (handlers event-loop)) handler)
    (values handler old)))

(defmethod deregister-handler :around ((handler handler) (event-loop event-loop))
  (bt:with-recursive-lock-held ((event-loop-lock event-loop))
    (call-next-method)))

(defmethod deregister-handler ((handler handler) (event-loop event-loop))
  (remhash (or (name handler) handler) (handlers event-loop))
  handler)

(defmethod deregister-handler ((name symbol) (event-loop event-loop))
  (deregister-handler (or (handler name event-loop)
                          (error "No such handler ~s." name))
                      event-loop))

(defun test-filter (filter event)
  (labels ((eval-test (test)
             (typecase test
               (cons
                (case (first test)
                  (and
                   (loop for form in (cdr test)
                         always (eval-test form)))
                  (or
                   (loop for form in (cdr test)
                         thereis (eval-test form)))
                  (not
                   (not (eval-test (second test))))
                  (T
                   (apply (first test)
                          (loop for form in (cdr test) collect (eval-test form))))))
               (null NIL)
               (symbol
                (let ((slot (or (find-class-slot-fuzzy test (class-of event))
                                (error "Don't know how to access the variable ~s in class ~s" test (class-of event)))))
                  (slot-value event (c2mop:slot-definition-name slot))))
               (T test))))
    (if filter
        (eval-test filter)
        T)))

(defmethod deliver-event-directly ((event event) (event-loop event-loop))
  (loop for handler being the hash-values of (handlers event-loop)
        do (when (and (typep event (event-type handler))
                      (test-filter (filter handler) event))
             (issue event handler))))

(defmethod handle :around ((event event) (event-loop event-loop))
  (bt:with-recursive-lock-held ((event-loop-lock event-loop))
    (call-next-method)))

(defmethod issue :before ((event event) (event-loop event-loop))
  (setf (issue-time event) (get-universal-time)))

(defmacro do-issue (event-type &rest args &key (loop '*standard-event-loop*) &allow-other-keys)
  (let ((args (removef args :loop)))
    `(issue (make-instance ',event-type ,@args :origin (here)) ,loop)))

(defclass sorted-event-loop (event-loop)
  ((sorted-handlers :initform () :accessor sorted-handlers)))

(defmethod register-handler :after ((handler handler) (event-loop sorted-event-loop))
  (ensure-handlers-sorted event-loop))

(defmethod deregister-handler :after ((handler handler) (event-loop sorted-event-loop))
  (setf (sorted-handlers event-loop) (remove handler (sorted-handlers event-loop))))

(defmethod deliver-event-directly ((event event) (event-loop sorted-event-loop))
  (loop for handler in (sorted-handlers event-loop)
        do (when (and (typep event (event-type handler))
                      (test-filter (filter handler) event))
             (issue event handler))))

(defmethod ensure-handlers-sorted ((event-loop sorted-event-loop))
  (setf (sorted-handlers event-loop)
        (sort-handlers (handlers event-loop) event-loop)))

(defmethod sort-handlers ((handlers hash-table) (event-loop sorted-event-loop))
  (sort-handlers (loop for v being the hash-values of handlers collect v)
                 event-loop))

(defmethod sort-handlers ((handlers list) (event-loop sorted-event-loop))
  ;; Graph time, yeah! We want to do a topological sort here.
  (let ((edges (make-hash-table :test 'eql))
        (nodes (make-hash-table :test 'eql))
        (sorted ()))
    ;; Build the graph
    (setf (gethash :start nodes) :unvisited)
    (setf (gethash :main nodes) :unvisited)
    (setf (gethash :end nodes) :unvisited)
    (push :main (gethash :start edges))
    (push :end (gethash :main edges))
    (dolist (handler handlers)
      (setf (gethash handler nodes) :unvisited)
      (push :end (gethash handler edges))
      (dolist (after (after handler))
        (push handler (gethash (handler after event-loop) edges)))
      (if (before handler)
          (dolist (before (before handler))
            (push (handler before event-loop) (gethash handler edges)))
          (push handler (gethash :main edges))))
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

(defclass compiled-event-loop (sorted-event-loop)
  ())

(defmethod register-handler :after ((handler handler) (event-loop compiled-event-loop))
  (recompile-event-loop event-loop))

(defmethod deregister-handler :after ((handler handler) (event-loop compiled-event-loop))
  (recompile-event-loop event-loop))

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
           collect (if (and (eql (first test) 'typep)
                            (eql (second test) 'ev))
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

(defmethod recompile-event-loop ((event-loop compiled-event-loop))
  (bt:with-recursive-lock-held ((event-loop-lock event-loop))
    (let ((loop-definition (build-event-loop (sorted-handlers event-loop) event-loop)))
      (setf (delivery-function event-loop) (compile-lambda loop-definition))))
  event-loop)

(defmethod build-event-loop ((handlers list) (event-loop compiled-event-loop))
  ;; Oh boy!
  (multiple-value-bind (cache inits filters) (extract-tests handlers)
    `(lambda (ev)
       (declare (optimize speed))
       (block NIL
         (let ,cache
           (declare (dynamic-extent ,@(loop for var in cache
                                            collect (if (listp var) (car var) var))))
           ,@inits
           ,@(loop for filter in filters
                   for handler in handlers
                   collect `(when ,filter
                              (issue ev ,handler))))))))

;; Initialise
(setf *standard-event-loop* (start (make-instance 'compiled-event-loop)))
