#|
 This file is a part of deeds
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.deeds)

(defun class-all-direct-slots (class)
  (let ((slots ()))
    (labels ((traverse (class)
               (dolist (slot (c2mop:class-direct-slots class))
                 (push slot slots)
                 (mapc #'traverse (c2mop:class-direct-superclasses class)))))
      (traverse (etypecase class
                  (standard-class class)
                  (symbol (find-class class)))))
    (nreverse slots)))

(defun find-slot-accessor (slot)
  (loop for writer in (c2mop:slot-definition-writers slot)
        thereis (find writer (c2mop:slot-definition-readers slot)
                      :test (lambda (a b) (if (listp a) (eql (second a) b) (eql a b))))))

(defun find-class-slot-fuzzy (slot-ish class)
  (flet ((name~= (a b)
           (or (eql a b) (string= a b))))
    (loop for slot in (class-all-direct-slots class)
          when (or (name~= slot-ish (c2mop:slot-definition-name slot))
                   (find slot-ish (c2mop:slot-definition-readers slot) :test #'name~=))
          do (return slot))))

(defun build-fuzzy-slot-accessor (slot-ish class instance)
  (let* ((slot (or (find-class-slot-fuzzy slot-ish class)
                   (error "Don't know how to access the variable ~s in class ~s" slot-ish class)))
         (accessor (find-slot-accessor slot)))
    (values
     (cond (accessor
            `(,accessor ,instance))
           (T
            `(slot-value ,instance ',(c2mop:slot-definition-name slot))))
     slot)))

(defmacro with-fuzzy-slot-bindings (vars (instance class) &body body)
  `(symbol-macrolet ,(loop for var in vars
                           collect (destructuring-bind (name &optional (slot-ish name)) (if (listp var) var (list var))
                                     `(,name ,(build-fuzzy-slot-accessor slot-ish class instance))))
     ,@body))

(defun parse-into-kargs-and-body (body)
  (values (loop for list = body then rest
                for (key val . rest) = list
                while (and (cdr list) (keywordp key))
                collect key collect val
                finally (setf body list))
          body))

(defun copy-hash-table (old)
  (let ((new (make-hash-table :test (hash-table-test old))))
    (maphash (lambda (k v) (setf (gethash k new) v)) old)
    new))
