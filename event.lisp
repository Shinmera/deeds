#|
 This file is a part of deeds
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.deeds)

(defclass event-class (cached-slots-class)
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
    (format stream "~@[~a ~]~s ~a ~@[~a~]" (when (issue-time event) (format-time (issue-time event))) :origin (origin event) (and (cancelled event) :cancelled))))

(defmacro define-event (name direct-superclasses direct-slots &rest options)
  (when (loop for super in direct-superclasses
              never (c2mop:subclassp (find-class super) (find-class 'event)))
    (push 'event direct-superclasses))
  (pushnew `(:metaclass event-class) options
           :test #'(lambda (a b) (eql (car a) (car b))))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)))

(defgeneric cancel (event)
  (:method ((event event))
    (setf (cancelled event) T)))

(define-event blocking-event ()
  ())

(define-event message-event ()
  ((message :initarg :message :reader message))
  (:default-initargs
   :message (error "MESSAGE required.")))

(define-event info-event (message-event)
  ())

(define-event warning-event (message-event)
  ())

(define-event error-event (message-event)
  ())

(define-event payload-event ()
  ((payload :initarg :payload :reader payload))
  (:default-initargs
   :payload (error "PAYLOAD required.")))

(define-event sequence-event ()
  ((index :initarg :index :reader index)
   (max-index :initarg :max-index :reader max-index))
  (:default-initargs
   :index (error "INDEX required.")
   :max-index NIL))

(define-event chunked-payload-event (payload-event sequence-event)
  ())

(define-event identified-event ()
  ((identifier :initarg :identifier :reader identifier)))

(defmethod initialize-instance :after ((identified-event identified-event) &key)
  (unless (slot-boundp identified-event 'identifier)
    (setf (slot-value identified-event 'identifier)
          identified-event)))

(define-event stream-event (identified-event)
  ())

(define-event stream-begin-event (stream-event)
  ())

(define-event stream-payload-event (stream-event payload-event)
  ()
  (:default-initargs
   :identifier (error "IDENTIFIER required.")))

(define-event stream-end-event (stream-event)
  ()
  (:default-initargs
   :identifier (error "IDENTIFIER required.")))
