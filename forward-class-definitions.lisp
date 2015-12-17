#|
 This file is a part of deeds
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.deeds)

(defclass event-class (standard-class) ())
(defclass event-slot () ())
(defclass event-direct-slot-definition (event-slot c2mop:standard-direct-slot-definition) ())
(defclass event-effective-slot-definition (event-slot c2mop:standard-effective-slot-definition) ())
;; (defclass event () ())

(defclass event-delivery () ())
(defclass queued-event-delivery (event-delivery simple-tasks:queued-runner) ())
(defclass event-loop (queued-event-delivery) ())
(defclass handler (event-delivery) ())
(defclass parallel-handler (handler) ())
(defclass queued-handler (handler queued-event-delivery) ())
(defclass locally-blocking-handler (handler) ())
(defclass globally-blocking-handler (handler queued-event-delivery) ())
(defclass event-task (simple-tasks:task) ())
(defclass blocking-event-task (event-task) ())

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

(define-condition event-loop-condition ()
  ((event-loop :initarg :event-loop :accessor event-loop-condition-event-loop)))

(define-condition event-loop-handler-dependency-cycle-error (error event-loop-condition)
  ((handler :initarg :handler :accessor event-loop-condition-handler))
  (:report (lambda (c s) (format s "Dependency cycle for handler ~a in ~a."
                                 (event-loop-condition-event-loop c)
                                 (event-loop-condition-handler c)))))

(define-condition handler-condition ()
  ((handler :initarg :handler :accessor handler-condition-handler)))

(define-condition handler-thread-stop-failed-warning (warning handler-condition)
  ((thread :initarg :thread :accessor handler-condition-thread))
  (:report (lambda (c s) (format s "Thread ~a of handler ~a did not stop."
                                 (handler-condition-thread c)
                                 (handler-condition-handler c)))))
