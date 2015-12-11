#|
 This file is a part of deeds
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.deeds)

(defgeneric start (event-delivery))
(defgeneric stop (event-delivery))
(defgeneric issue (event event-delivery))
(defgeneric handle (event event-delivery))
(defgeneric handler (handler event-loop))
(defgeneric register-handler (handler event-loop))
(defgeneric deregister-handler (handler event-loop))
(defgeneric sort-handlers (handlers event-loop))
(defgeneric ensure-handlers-sorted (event-loop))
(defgeneric build-event-loop (event-loop))
(defgeneric deliver (event handler event-loop))

(define-condition event-loop-condition ()
  ((event-loop :initarg :event-loop :accessor event-loop-condition-event-loop)))

(define-condition event-loop-handler-dependency-cycle-error (error event-loop-condition)
  ((handler :initarg :handler :accessor event-loop-condition-handler))
  (:report (lambda (c s) (format s "Dependency cycle for handler ~a in ~a."
                                 (event-loop-condition-event-loop c)
                                 (event-loop-condition-handler c)))))

(defclass event-delivery (simple-tasks:queued-runner)
  ((delivery-function :initarg :delivery-function :accessor delivery-function))
  (:default-initargs
   :delivery-function #'print))

(defmethod print-object ((event-delivery event-delivery) stream)
  (print-unreadable-object (event-delivery stream :type T :identity T)
    (format stream "~s" (simple-tasks:status event-delivery))))

(defclass event-loop (event-delivery)
  ((handlers :initform (make-hash-table :test 'eql) :accessor handlers)
   (sorted-handlers :initform () :accessor sorted-handlers)
   (lock :initform (bt:make-recursive-lock "Event loop lock") :accessor event-loop-lock)))

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

(defclass blocking-handler (handler)
  ())

(defclass event-task (simple-tasks:task)
  ((event :initarg :event :accessor event-task-event)))

(defmethod simple-tasks:run-task ((event-task event-task))
  (handle (event-task-event event-task) (simple-tasks:runner event-task)))

(defmethod start ((event-delivery event-delivery))
  (simple-tasks:make-runner-thread event-delivery)
  event-delivery)

(defmethod stop ((event-delivery event-delivery))
  (simple-tasks:stop-runner event-delivery)
  event-delivery)

(defmethod issue ((event event) (event-delivery event-delivery))
  (setf (issue-time event) (get-universal-time))
  (simple-tasks:schedule-task
   (make-instance 'event-task :event event) event-delivery)
  event)

(defmethod handle ((event event) (event-delivery event-delivery))
  (funcall (delivery-function event-delivery) event))

(defmethod handler ((name symbol) (event-loop event-loop))
  (gethash name (handlers event-loop)))

(defmethod handler ((handler handler) (event-loop event-loop))
  (if (name handler)
      (handler (name handler) event-loop)
      (gethash handler (handlers event-loop))))

(defmethod register-handler ((handler handler) (event-loop event-loop))
  (let ((old (handler handler event-loop)))
    (setf (gethash (or (name handler) handler) (handlers event-loop)) handler)
    (ensure-handlers-sorted event-loop)
    (let* ((loop-definition (build-event-loop event-loop))
           (compiled-loop (compile NIL loop-definition)))
      (bt:with-recursive-lock-held ((event-loop-lock event-loop))
        (when old (stop old))
        (start handler)
        (setf (delivery-function event-loop) compiled-loop))))
  handler)

(defmethod deregister-handler ((handler handler) (event-loop event-loop))
  (stop handler)
  (remhash (or (name handler) handler) (handlers event-loop))
  (setf (sorted-handlers) (remove handler (sorted-handlers event-loop)))
  (rebuild-event-loop event-loop)
  handler)

(defmethod ensure-handlers-sorted ((event-loop event-loop))
  (setf (sorted-handlers event-loop)
        (sort-handlers (loop for v being the hash-values of (handlers event-loop) collect v)
                       event-loop)))

(defmethod sort-handlers (handlers (event-loop event-loop))
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
                 (:uvisited
                   (setf (gethash node nodes) :temporary)
                   (dolist (target (gethash node edges))
                     (visit target))
                   (remhash node nodes)
                   (push node sorted)))))
      (loop while (with-hash-table-iterator (iterator nodes)
                    (multiple-value-bind (found node) (iterator)
                      (when found (visit node) T)))))
    sorted))

(defmethod build-event-loop ((event-loop event-loop))
  ;; Oh boy!
  `(lambda (ev)
     ))

(defmethod handle :around ((event event) (event-loop event-loop))
  (bt:with-recursive-lock-held ((event-loop-lock event-loop))
    (call-next-method)))

(defmethod deliver ((event event) (handler handler) (event-loop event-loop))
  (issue event handler))

;; We simply bypass default actions and run in the same thread to block.
(defmethod start ((blocking-handler blocking-handler))
  blocking-handler)

(defmethod stop ((blocking-handler blocking-handler))
  blocking-handler)

(defmethod issue ((event event) (blocking-handler blocking-handler))
  (handle event blocking-handler))

(defmacro define-handler ((name event-type) args &body options-and-body)
  (destructuring-bind (ev &rest args) args
    (multiple-value-bind (options body) (parse-into-kargs-and-body options-and-body)
      (destructuring-bind (&rest options &key (loop '*standard-event-loop*) (class ''handler) &allow-other-keys) options
        (let ((options (copy-list options)))
          (remf options :loop)
          (remf options :class)
          `(register-handler
            (make-instance
             ,class
             ,@options
             :name ',name
             :event-type ',event-type
             :delivery-function
             (lambda (,ev)
               (with-fuzzy-slot-bindings ,args (,ev ,event-type)
                 ,@body)))
            ,loop))))))
