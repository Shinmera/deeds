(in-package #:org.shirakumo.deeds)

(defclass cached-slots-class (standard-class)
  ((class-all-direct-slots :initform NIL :accessor class-all-direct-slots)))

(defmethod c2mop:validate-superclass ((class cached-slots-class) (superclass t))
  NIL)

(defmethod c2mop:validate-superclass ((class standard-class) (superclass cached-slots-class))
  T)

(defmethod c2mop:validate-superclass ((class cached-slots-class) (superclass standard-class))
  T)

(defmethod c2mop:validate-superclass ((class cached-slots-class) (superclass cached-slots-class))
  T)

(defmethod initialize-instance :after ((class cached-slots-class) &key)
  (setf (class-all-direct-slots class)
        (compute-all-direct-slots class)))

(defmethod reinitialize-instance :after ((class cached-slots-class) &key)
  (setf (class-all-direct-slots class)
        (compute-all-direct-slots class)))

(defmethod class-all-direct-slots ((class standard-class))
  ;; Slow path!
  (compute-all-direct-slots class))

(defmethod class-all-direct-slots ((name symbol))
  (class-all-direct-slots (find-class name)))

(defun compute-all-direct-slots (class)
  (let ((slots ()))
    (labels ((traverse (class)
               (when (typep class 'standard-class)
                 (c2mop:finalize-inheritance class)
                 (dolist (slot (c2mop:class-direct-slots class))
                   (push slot slots))
                 (loop for super in (c2mop:class-direct-superclasses class)
                       do (if (typep class 'cached-slots-class)
                              (dolist (s (class-all-direct-slots super)) (push s slots))
                              (traverse super))))))
      (traverse (etypecase class
                  (standard-class class)
                  (symbol (find-class class)))))
    (nreverse slots)))

(defmacro define-cached-slots-class (name direct-superclasses direct-slots &rest options)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name ,direct-superclasses ,direct-slots
       ,@options
       (:metaclass cached-slots-class))))
