#|
 This file is a part of deeds
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.deeds)

(start (setf *standard-event-loop* (make-instance 'event-loop)))

(defmacro do-issue (event-type &rest args &key (loop '*standard-event-loop*) &allow-other-keys)
  (let ((args (removef args :loop)))
    `(issue (make-instance ',event-type ,@args :origin (here)) ,loop)))

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
