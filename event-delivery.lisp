#|
 This file is a part of deeds
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.deeds)

(defgeneric start (event-delivery))
(defgeneric stop (event-delivery))
(defgeneric issue (event event-delivery))
(defgeneric handle (event event-delivery))

(defclass event-delivery ()
  ((delivery-function :initarg :delivery-function :accessor delivery-function))
  (:default-initargs
   :delivery-function #'print))

(defmethod start ((event-delivery event-delivery))
  event-delivery)

(defmethod stop ((event-delivery event-delivery))
  event-delivery)

(defmethod issue ((event event) (event-delivery event-delivery))
  (handle event event-delivery)
  event)

(defmethod handle ((event event) (event-delivery event-delivery))
  (funcall (delivery-function event-delivery) event))

(defclass queued-event-delivery (event-delivery simple-tasks:queued-runner)
  ())

(defmethod start ((event-delivery queued-event-delivery))
  (when (simple-tasks:status= event-delivery :runing)
    (cerror "Start anyway" "~a is already started!" event-delivery))
  (simple-tasks:make-runner-thread event-delivery)
  event-delivery)

(defmethod stop ((event-delivery queued-event-delivery))
  (simple-tasks:stop-runner event-delivery)
  event-delivery)

(defmethod issue ((event event) (event-delivery queued-event-delivery))
  (simple-tasks:schedule-task
   (make-instance 'event-task :event event) event-delivery)
  event)

(defmethod issue ((blocking-event blocking-event) (event-delivery queued-event-delivery))
  (simple-tasks:schedule-task
   (make-instance 'blocking-event-task :event blocking-event) event-delivery)
  blocking-event)

(defclass event-task (simple-tasks:task)
  ((event :initarg :event :accessor event-task-event)))

(defmethod simple-tasks:run-task ((event-task event-task))
  (handle (event-task-event event-task) (simple-tasks:runner event-task)))

(defclass blocking-event-task (event-task simple-tasks:blocking-task)
  ())
