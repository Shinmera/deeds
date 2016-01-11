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
   (handle-cancelled :initarg :handle-cancelled :reader handle-cancelled))
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
                          :delivery-function
                          (lambda (,ev)
                            (declare (ignorable ,ev))
                            (with-origin (',name)
                              (with-fuzzy-slot-bindings ,args (,ev ,event-type)
                                ,@body)))))
             (start ,self)
             (multiple-value-bind (,self ,old) (register-handler ,self ,loop)
               (when ,old (stop ,old))
               (values ,self ,old))))))))

(defmacro with-one-time-handler (event-type args &body options-and-body)
  (multiple-value-bind (options body) (parse-into-kargs-and-body options-and-body)
    (let ((self (or (getf options :self) (gensym "SELF")))
          (loop (or (getf options :loop) '*standard-event-loop*)))
      `(define-handler (NIL ,event-type) ,args
         :self ,self
         ,@options
         (unwind-protect
              (progn ,@body)
           (unwind-protect
                (deregister-handler ,self ,loop)
             (stop ,self)))))))
