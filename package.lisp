#|
 This file is a part of deeds
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:deeds
  (:nicknames #:org.shirakumo.deeds)
  (:use #:cl)
  ;; event-delivery.lisp
  (:export
   #:start
   #:stop
   #:issue
   #:handle
   #:event-delivery
   #:delivery-function
   #:queued-event-delivery
   #:event-task
   #:event-task-event)
  ;; event-loop.lisp
  (:export
   #:handler
   #:register-handler
   #:deregister-handler
   #:sort-handlers
   #:ensure-handlers-sorted
   #:build-event-loop
   #:recompile-event-loop
   #:event-loop
   #:handlers
   #:sorted-handlers
   #:lock
   #:*standard-event-loop*
   #:do-issue)
  ;; event.lisp
  (:export
   #:event-class
   #:event-slot
   #:event-slot-mutable
   #:event-direct-slot-definition
   #:event-effective-slot-definition
   #:event
   #:issue-time
   #:origin
   #:cancelled
   #:define-event
   #:cancel
   #:message-event
   #:message
   #:info-event
   #:warning-event
   #:error-event
   #:payload-event
   #:payload
   #:sequence-event
   #:index
   #:chunked-payload-event
   #:max-index
   #:identified-event
   #:identifier
   #:stream-event
   #:stream-begin-event
   #:stream-payload-event
   #:stream-end-event)
  ;; forward-class-definitions.lisp
  (:export
   #:event-condition
   #:event-condition-event
   #:immutable-event-slot-modified
   #:event-condition-slot
   #:event-condition-value
   #:immutable-event-slot-has-writer
   #:event-condition-slot
   #:event-condition-writers
   #:event-loop-condition
   #:event-loop-condition-event-loop
   #:event-loop-handler-dependency-cycle-error
   #:event-loop-condition-handler
   #:handler-condition
   #:handler-condition-handler
   #:handler-thread-stop-failed-warning
   #:handler-condition-thread)
  ;; handler.lisp
  (:export
   #:handler
   #:name
   #:event-type
   #:filter
   #:before
   #:after
   #:parallel-handler
   #:threads
   #:lock
   #:queued-handler
   #:locally-blocking-handler
   #:globally-blocking-handler
   #:blocking-event-task
   #:define-handler
   #:with-one-time-handler)
  ;; origin.lisp
  (:export
   #:*origin*
   #:with-origin
   #:here)
  ;; toolkit.lisp
  (:export
   #:with-fuzzy-slot-bindings))
