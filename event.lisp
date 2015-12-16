#|
 This file is a part of deeds
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.deeds)

(define-condition event-condition (condition)
  ((event :initarg :event :accessor event-condition-event)))

(define-condition immutable-event-slot-modified (error event-condition)
  ((slot :initarg :slot :accessor event-condition-slot)
   (value :initarg :value :accessor event-condition-value))
  (:report (lambda (c s) (format s "Attempted to write ~s to slot ~a of ~a."
                                 (event-condition-value c) (event-condition-slot c) (event-condition-event c)))))

(define-condition immutable-event-slot-has-writer (warning event-condition)
  ((slot :initarg :slot :accessor event-condition-slot)
   (writers :initarg :writers :accessor event-condition-writers))
  (:report (lambda (c s) (format s "Defining writers ~s to an immutable slot ~a of ~a."
                                 (event-condition-writers c) (event-condition-slot c) (event-condition-event c)))))

(defclass event-class (standard-class)
  ())

(defmethod c2mop:validate-superclass ((class event-class) (superclass t))
  NIL)

(defmethod c2mop:validate-superclass ((class standard-class) (superclass event-class))
  T)

(defmethod c2mop:validate-superclass ((class event-class) (superclass standard-class))
  T)

(defmethod c2mop:validate-superclass ((class event-class) (superclass event-class))
  T)

(defclass event-slot ()
  ((mutable :initarg :mutable :initform NIL :accessor event-slot-mutable))
  (:documentation "Superclass for event slots with an option"))

(defclass event-direct-slot-definition (event-slot c2mop:standard-direct-slot-definition)
  ())

(defclass event-effective-slot-definition (event-slot c2mop:standard-effective-slot-definition)
  ())

(defun check-event-slots (class slot-forms)
  (dolist (form slot-forms)
    (when (and (not (getf form :mutable)) (getf form :writers))
      (warn 'immutable-event-slot-has-writer :event class :slot (getf form :name) :writers (getf form :writers)))))

(defmethod initialize-instance :before ((class event-class) &key direct-slots &allow-other-keys)
  (check-event-slots class direct-slots))

(defmethod reinitialize-instance :before ((class event-class) &key direct-slots &allow-other-keys)
  (check-event-slots class direct-slots))

(defmethod c2mop:direct-slot-definition-class ((class event-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'event-direct-slot-definition))

(defmethod c2mop:effective-slot-definition-class ((class event-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'event-effective-slot-definition))

(defmethod c2mop:compute-effective-slot-definition ((class event-class) name direct-slots)
  (declare (ignore name))
  (let ((effective-slot (call-next-method)))
    (dolist (direct-slot direct-slots)
      (when (and (typep direct-slot 'event-direct-slot-definition)
                 (eql (c2mop:slot-definition-name direct-slot)
                      (c2mop:slot-definition-name effective-slot)))
        (setf (event-slot-mutable effective-slot)
              (event-slot-mutable direct-slot))
        (return)))
    effective-slot))

(defmethod (setf c2mop:slot-value-using-class) :before (value (class event-class) event (slotd event-slot))
  (unless (event-slot-mutable slotd)
    (cerror "Write to the slot anyway." 'immutable-event-slot-modified
            :event event :slot (c2mop:slot-definition-name slotd) :value value)))

(defclass event ()
  ((issue-time :accessor issue-time :initform NIL :mutable T)
   (origin :initarg :origin :initform *origin* :reader origin :mutable NIL)
   (cancelled :initform NIL :accessor cancelled :mutable T))
  (:metaclass event-class))

(defmethod initialize-instance :around ((event event) &key)
  (handler-bind ((immutable-event-slot-modified #'continue))
    (call-next-method)))

(defmethod print-object ((event event) stream)
  (print-unreadable-object (event stream :type T :identity T)
    (format stream "~@[~a ~]~s ~a" (when (issue-time event) (format-time (issue-time event))) :origin (origin event))))

(defmacro define-event (name direct-superclasses direct-slots &rest options)
  (when (loop for super in direct-superclasses
              never (c2mop:subclassp (find-class super) (find-class 'event)))
    (push 'event direct-superclasses))
  (pushnew `(:metaclass event-class) options
           :test #'(lambda (a b) (eql (car a) (car b))))
  `(defclass ,name ,direct-superclasses
     ,direct-slots
     ,@options))

(defgeneric cancel (event)
  (:method ((event event))
    (setf (cancelled event) T)))
