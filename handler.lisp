#|
 This file is a part of deeds
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.deeds)

(defclass handler (event-delivery)
  ((name :initarg :name :reader name)
   (event-type :initarg :event-type :reader event-type)
   (filter :initarg :filter :reader filter)
   (before :initarg :before :reader before)
   (after :initarg :after :reader after)
   (handle-cancelled :initarg :handle-cancelled :reader handle-cancelled)
   (loops :initform NIL :accessor loops))
  (:default-initargs
   :name NIL
   :event-type 'event
   :filter NIL
   :before ()
   :after ()
   :handle-cancelled NIL))

(defmethod print-object ((handler handler) stream)
  (print-unreadable-object (handler stream :type T :identity T)
    (format stream "~s ~s" :name (name handler))))

(defmethod handle :around ((event event) (handler handler))
  (when (or (not (cancelled event))
            (handle-cancelled handler))
    (call-next-method)))

(defclass parallel-handler (handler)
  ((threads :initform () :accessor threads)
   (handler-lock :initform (bt:make-recursive-lock "parallel-handler lock") :accessor handler-lock)))

(defmethod issue ((event event) (parallel-handler parallel-handler))
  (bt:with-lock-held ((handler-lock parallel-handler))
    (let (thread)
      (setf thread (bt:make-thread (lambda ()
                                     (unwind-protect
                                          (with-simple-restart (abort "Stop the handler thread.")
                                            (handle event parallel-handler))
                                       (bt:with-lock-held ((handler-lock parallel-handler))
                                         (setf (threads parallel-handler)
                                               (remove thread (threads parallel-handler))))))
                                   :name (format NIL "~a thread" parallel-handler)))
      (push thread (threads parallel-handler)))))

(defmethod issue ((blocking-event blocking-event) (parallel-handler parallel-handler))
  (handle blocking-event parallel-handler))

(defmethod stop ((parallel-handler parallel-handler))
  (loop for thread = (bt:with-lock-held ((handler-lock parallel-handler))
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

(defclass globally-blocking-handler (handler)
  ((handler-lock :initform (bt:make-recursive-lock "globally-blocking-handler lock") :accessor handler-lock)))

(defmethod handle ((event event) (globally-blocking-handler globally-blocking-handler))
  (bt:with-recursive-lock-held ((handler-lock globally-blocking-handler))
    (call-next-method)))

(defmacro define-handler ((name event-type) args &body options-and-body)
  (destructuring-bind (ev &rest args) args
    (multiple-value-bind (options body) (parse-into-kargs-and-body options-and-body)
      (destructuring-bind (&rest options &key (loop '*standard-event-loop*) (class ''queued-handler) filter (self (gensym "SELF")) &allow-other-keys) options
        (let ((options (removef options :loop :class :filter :self))
              (old (gensym "OLD-HANDLER")))
          `(let (,self)
             (setf ,self (make-instance
                          ,class
                          ,@options
                          :name ',name
                          :event-type ',event-type
                          :filter ',filter
                          ,@(when body
                              `(:delivery-function
                                (lambda (,ev)
                                  (declare (ignorable ,ev))
                                  (with-origin (',name)
                                    (with-fuzzy-slot-bindings ,args (,ev ,event-type)
                                      ,@body)))))))
             (start ,self)
             ,(if loop
                  `(multiple-value-bind (,self ,old) (register-handler ,self ,loop)
                     (when ,old (stop ,old))
                     (values ,self ,old))
                  self)))))))

(defclass one-time-handler (handler)
  ((handler-lock :initform (bt:make-lock "one-time-handler-lock") :accessor handler-lock)
   (thread :initform NIL :accessor thread)))

(defmethod handle ((event event) (handler one-time-handler))
  (when (bt:acquire-lock (handler-lock handler))
    (setf (thread handler)
          (bt:make-thread (lambda ()
                            (funcall (delivery-function handler) event))))))

(defmacro with-one-time-handler (event-type args &body options-and-body)
  (multiple-value-bind (options body) (parse-into-kargs-and-body options-and-body)
    (let ((self (or (getf options :self) (gensym "SELF")))
          (loop (or (getf options :loop) '*standard-event-loop*)))
      `(define-handler (NIL ,event-type) ,args
         ,@options
         :self ,self
         :class 'one-time-handler
         (unwind-protect
              (progn ,@body)
           (unwind-protect
                (deregister-handler ,self ,loop)
             (stop ,self)))))))

(defclass condition-notify-handler (one-time-handler)
  ((condition-variable :initform (bt:make-condition-variable) :accessor condition-variable)
   (issue-synchronizer-lock :initform (bt:make-lock) :accessor issue-synchronizer-lock)
   (response-event :initform NIL :accessor response-event)))

(defmethod handle ((event event) (handler condition-notify-handler))
  (setf (event handler) ev)
  ;; Quickly access lock to make sure the issuer has
  ;; entered the condition-wait.
  (bt:with-lock-held ((issue-synchronizer-lock handler)))
  (bt:condition-notify (condition-variable handler)))

(defmacro with-response (issue response (&key filter timeout (loop '*standard-event-loop*)) &body body)
  (let ((handler (gensym "HANDLER")))
    (destructuring-bind (issue-event &rest issue-args) (ensure-list issue)
      (destructuring-bind (response-event &optional (event 'ev) &rest response-args) (ensure-list response)
        `(let ((,handler (make-instance
                          'condition-notify-handler
                          :event-type ',response-event
                          :filter ',filter)))
           (start ,handler)
           (unwind-protect
                (bt:with-lock-held ((issue-synchronizer-lock ,handler))
                  (register-handler ,handler ,loop)
                  (do-issue ,issue-event ,@issue-args)
                  (when (bt:condition-wait (condition-variable ,handler)
                                           (issue-synchronizer-lock ,handler)
                                           :timeout ,timeout)
                    (let ((,event (response-event ,handler)))
                      (declare (ignorable ,event))
                      (with-fuzzy-slot-bindings ,response-args (,event ,response-event)
                        ,@body))))
             (deregister-handler ,handler ,loop)
             (stop ,handler)))))))
