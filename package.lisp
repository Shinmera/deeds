(in-package #:cl-user)
(defpackage #:deeds
  (:nicknames #:org.shirakumo.deeds)
  (:use #:cl)
  ;; class-slots.lisp
  (:export
   #:cached-slots-class
   #:class-all-direct-slots
   #:compute-all-direct-slots
   #:define-cached-slots-class)
  ;; command.lisp
  (:export
   #:command-event
   #:define-command)
  ;; event-delivery.lisp
  (:export
   #:running
   #:start
   #:stop
   #:issue
   #:handle
   #:event-delivery
   #:delivery-function
   #:queued-event-delivery)
  ;; event-loop.lisp
  (:export
   #:*standard-event-loop*
   #:handler
   #:register-handler
   #:deregister-handler
   #:deliver-event-directly
   #:sort-handlers
   #:ensure-handlers-sorted
   #:build-event-loop
   #:recompile-event-loop
   #:event-loop
   #:handlers
   #:event-loop-lock
   #:test-filter
   #:do-issue
   #:broadcast
   #:sorted-event-loop
   #:sorted-handlers
   #:compiled-event-loop)
  ;; event.lisp
  (:export
   #:event-class
   #:event-slot
   #:event-slot-mutable
   #:event-direct-slot-definition
   #:event-effective-slot-definition
   #:with-immutable-slots-unlocked
   #:event
   #:event-loop
   #:issue-time
   #:origin
   #:cancelled
   #:define-event
   #:cancel
   #:blocking-event
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
   #:loops
   #:handle-cancelled
   #:parallel-handler
   #:threads
   #:handler-lock
   #:queued-handler
   #:locally-blocking-handler
   #:globally-blocking-handler
   #:make-handler
   #:with-handler
   #:define-handler
   #:one-time-handler
   #:with-one-time-handler
   #:condition-notify-handler
   #:response-event
   #:with-awaiting
   #:with-response)
  ;; origin.lisp
  (:export
   #:*origin*
   #:with-origin
   #:here)
  ;; toolkit.lisp
  (:export
   #:with-fuzzy-slot-bindings))
