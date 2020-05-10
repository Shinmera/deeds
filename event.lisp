#|
 This file is a part of deeds
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.deeds)

(defclass event ()
  ((origin :initarg :origin :reader origin :mutable NIL)
   (event-loop :initform NIL :reader event-loop :mutable NIL)
   (issue-time :initform NIL :accessor issue-time :mutable T)
   (cancelled :initform NIL :accessor cancelled :mutable T))
  (:metaclass event-class)
  (:default-initargs
   :origin *origin*))

(defmethod initialize-instance :around ((event event) &key)
  (with-immutable-slots-unlocked ()
    (call-next-method)))

(defmethod print-object ((event event) stream)
  (print-unreadable-object (event stream :type T :identity T)
    (format stream "~@[~a ~]~s ~a~@[ ~a~]" (when (issue-time event) (format-time (issue-time event))) :origin (origin event) (and (cancelled event) :cancelled))))

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
