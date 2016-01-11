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
           (make-field (a)
             (destructuring-bind (name &optional value &rest kargs) (ensure-list a)
               `(,name :initform ,value :initarg ,(keyword name) ,@kargs))))
    (let ((pure-args (mapcar #'unlist (remove-if #'lambda-keyword-p args))))
      (multiple-value-bind (options body) (parse-into-kargs-and-body options-and-body)
        (destructuring-bind (&rest options &key superclasses loop &allow-other-keys) options
          `(progn
             (define-event ,name (command-event ,@superclasses)
               ,(mapcar #'make-field (cdr (remove-if #'lambda-keyword-p args))))
             (defun ,name ,(cdr args)
               (do-issue ,name 
                 :loop ,loop
                 ,@(loop for var in (cdr pure-args)
                         collect (keyword var) collect var)))
             (define-handler (,name ,name) ,pure-args
               ,@(removef options :superclasses)
               ,@body)))))))
