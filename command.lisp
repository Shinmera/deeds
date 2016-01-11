#|
 This file is a part of deeds
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.deeds)

(define-event command-event ()
  ())

(defmacro define-command (name args &body options-and-body)
  (labels ((keyword (a) (intern (string a) :keyword))
           (lambda-keyword-p (a) (find a lambda-list-keywords))
           (ensure-list (a) (if (listp a) a (list a)))
           (unlist (a) (if (listp a) (car a) a))
           (make-req-field (a)
             (destructuring-bind (name &rest kargs) (ensure-list a)
               `(,name :initarg ,(keyword name) :initform (error ,(format NIL "~a required." name)) ,@kargs)))
           (make-opt-field (a)
             (destructuring-bind (name &optional value &rest kargs) (ensure-list a)
               `(,name :initarg ,(keyword name) :initform ,value ,@kargs)))
           (make-opt-arg (a)
             (destructuring-bind (name &optional value &rest kargs) (ensure-list a)
               (declare (ignore kargs))
               `(,name ,value))))
    (let ((pure-args (mapcar #'unlist (remove-if #'lambda-keyword-p args))))
      (lambda-fiddle:with-destructured-lambda-list (:required required :optional optional :rest rest :key key) (cdr args)
        (multiple-value-bind (options body) (parse-into-kargs-and-body options-and-body)
          (destructuring-bind (&rest options &key superclasses loop &allow-other-keys) options
            `(progn
               (define-event ,name (command-event ,@superclasses)
                 (,@(mapcar #'make-req-field required)
                  ,@(mapcar #'make-opt-field optional)
                  ,@(when rest (list (make-req-field rest)))
                  ,@(mapcar #'make-opt-field key)))
               (defun ,name (,@(mapcar #'unlist required)
                             ,@(when optional `(&optional ,@(mapcar #'make-opt-arg optional)))
                             ,@(when rest `(&rest ,rest))
                             ,@(when key `(&key ,@(mapcar #'make-opt-arg key))))
                 (do-issue ,name 
                   :loop ,loop
                   ,@(loop for var in (cdr pure-args)
                           collect (keyword var) collect var)))
               (define-handler (,name ,name) ,pure-args
                 ,@(removef options :superclasses)
                 ,@body))))))))
