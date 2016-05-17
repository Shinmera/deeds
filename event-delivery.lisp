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

(defmethod print-object ((event-delivery event-delivery) stream)
  (print-unreadable-object (event-delivery stream :type T :identity T)
    (format stream "~@[RUNNING~]" (running event-delivery))))

(defmethod running ((stuff list))
  (every #'running stuff))

(defmethod running ((event-delivery event-delivery))
  T)

(defmethod start ((stuff list))
  (mapc #'start stuff))

(defmethod start ((event-delivery event-delivery))
  event-delivery)

(defmethod stop ((stuff list))
  (mapc #'stop stuff))

(defmethod stop ((event-delivery event-delivery))
  event-delivery)

(defmethod issue ((event event) (event-delivery event-delivery))
  (handle event event-delivery)
  event)

(defmethod handle ((event event) (event-delivery event-delivery))
  (funcall (delivery-function event-delivery) event))

(defclass queued-event-delivery (event-delivery)
  ((front-queue :initform (make-array 100 :adjustable T :fill-pointer 0) :accessor front-queue)
   (back-queue :initform (make-array 100 :adjustable T :fill-pointer 0) :accessor back-queue)
   (queue-condition :initform (bt:make-condition-variable :name "QUEUE-CONDITION") :reader queue-condition)
   (queue-lock :initform (bt:make-lock "QUEUE-LOCK") :reader queue-lock)
   (queue-thread :initform NIL :accessor queue-thread)))

(defmethod running ((event-delivery queued-event-delivery))
  (not (null (queue-thread event-delivery))))

(defmethod start ((event-delivery queued-event-delivery))
  (unless (running event-delivery)
    (bt:with-lock-held ((queue-lock event-delivery))
      (setf (queue-thread event-delivery)
            (bt:make-thread (lambda () (process-delivery-queue event-delivery))))))
  event-delivery)

(defmethod stop ((event-delivery queued-event-delivery))
  (when (running event-delivery)
    (let ((thread (queue-thread event-delivery)))
      (setf (queue-thread event-delivery) NIL)
      (bt:condition-notify (queue-condition event-delivery))
      (loop while (bt:thread-alive-p thread)
            for i from 1
            do (sleep 0.001)
               (when (= 0 (mod i 100))
                 (restart-case (error "Queue thread does not seem to be shutting down gracefully.")
                   (continue ()
                     :report "Continue waiting.")
                   (abort ()
                     :report "Try to forcibly terminate the thread."
                     (bt:destroy-thread thread)
                     (return)))))))
  event-delivery)

(defmethod issue ((event event) (event-delivery queued-event-delivery))
  (bt:with-lock-held ((queue-lock event-delivery))
    (vector-push-extend event (front-queue event-delivery)))
  (bt:condition-notify (queue-condition event-delivery))
  event)

(defmethod issue ((event blocking-event) (event-delivery queued-event-delivery))
  ;; FIXME
  blocking-event)

(defmethod process-delivery-queue ((event-delivery queued-event-delivery))
  (unwind-protect
       (loop (loop while (< 0 (length (front-queue event-delivery)))
                   do (rotatef (front-queue event-delivery)
                               (back-queue event-delivery))
                      (let ((queue (back-queue event-delivery)))
                        (loop for i from 0 below (length queue)
                              do (handle (aref queue i) event-delivery)
                                 (setf (aref queue i) NIL))
                        (setf (fill-pointer queue) 0)))
             (bt:with-lock-held ((queue-lock event-delivery))
               (bt:condition-wait (queue-condition event-delivery)
                                  (queue-lock event-delivery)
                                  :timeout 0.1))
             (unless (queue-thread event-delivery)
               (return)))
    (setf (queue-thread event-delivery) NIL)))
