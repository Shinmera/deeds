#|
 This file is a part of deeds
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.deeds)

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

(defun find-class-slot-for-compound (slot-ish compound)
  (etypecase compound
    (cons
     (ecase (first compound) (and) (or))
     (some (lambda (a) (find-class-slot-for-compound slot-ish a)) (rest compound)))
    (symbol (find-class-slot-fuzzy slot-ish compound))))

(defun build-fuzzy-slot-accessor (slot-ish class instance)
  (let* ((slot (or (find-class-slot-for-compound slot-ish class)
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

(defun copy-hash-table (old &key (test (hash-table-test old))
                                 (size (hash-table-size old))
                                 (rehash-size (hash-table-rehash-size old))
                                 (rehash-threshold (hash-table-rehash-threshold old)))
  (let ((new (make-hash-table :test test
                              :size size
                              :rehash-size rehash-size
                              :rehash-threshold rehash-threshold)))
    (maphash (lambda (k v) (setf (gethash k new) v)) old)
    new))

(defun format-time (universal-time)
  (multiple-value-bind (s m h dd mm yy) (decode-universal-time universal-time)
    (format NIL "~4,'0d.~2,'0d.~2,'0d ~2,'0d:~2,'0d:~2,'0d" yy mm dd h m s)))

(defun removef (list &rest remove-properties)
  (loop for (key val) on list by #'cddr
        for found = (find key remove-properties)
        unless found collect key
        unless found collect val))

(defun unlist (a)
  (if (listp a) (first a) a))

(defun ensure-list (a &rest elements)
  (if (listp a) a (list* a elements)))

(defun compile-lambda (lambda)
  (handler-bind ((style-warning #'muffle-warning)
                 #+sbcl (sb-ext:compiler-note #'muffle-warning))
    (compile NIL lambda)))
