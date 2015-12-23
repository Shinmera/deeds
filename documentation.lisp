#|
 This file is a part of deeds
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.deeds)

(defun checkdocs (&optional (package #.*package*))
  "Check that all functions, classes, and variables have docstrings."
  (do-symbols (symb package)
    (when (eql (symbol-package symb) package)
      (when (and (fboundp symb) (not (documentation symb 'function)))
        (warn "No documentation for function ~s." symb))
      (when (and (boundp symb) (not (documentation symb 'variable)))
        (warn "No documentation for variable ~s." symb))
      (when (and (find-class symb NIL) (not (documentation symb 'type)))
        (warn "No documentation for class ~s." symb)))))

(defmacro setdocs (&body pairs)
  "Easily set the documentation."
  `(progn
     ,@(loop for (var doc) in pairs
             collect (destructuring-bind (var &optional (type 'function))
                         (if (listp var) var (list var))
                       `(setf (documentation ',var ',type) ,doc)))))

(setdocs
  (start
   "Start the event delivery and make it ready to accept and deliver events.")
  
  (stop
   "Stop the event delivery to stop it from accepting and delivering events.")
  
  (issue
   "Issue an event to the delivery so that it is sent out to the handlers.

The exact point in time when the issued event is handled and processed
is not specified. However, the order in which events are handled must
be the same as the order in which they are issued. An event should only
ever be issued once. There is no check made to ensure this, but issuing
an event multiple times or on multiple deliveries leads to undefined
behaviour.")
  
  (handle
   "Directly handle the event by sending it to the handlers.

This function bypasses all threading, caching, ordering, and other
protective or optimisation constructs of the event delivery itself
and processes the event immediately. You should not call this function
on your own unless for when you are implementing an event-delivery on
your own. In that case you should call this function whenever you are
ready to actually process an event and send it out.

By default this simply calls the DELIVERY-FUNCTION of the
event-delivery with the given event.

See ISSUE")
  
  ((event-delivery type)
   "A base class for any kind of object that can deliver events.

This class must implement START, STOP, ISSUE, and HANDLE.

See DELIVERY-FUNCTION")
  
  (delivery-function
   "The function that is used to deliver events.

See HANDLE")
  
  ((queued-event-delivery type)
   "An event delivery that uses a queue and background thread to deliver events.

See EVENT-DELIVERY")
  
  ((event-task type)
   "A simple container to deliver events using the simple-tasks framework.

See SIMPLE-TASKS:TASK
See EVENT-TASK-EVENT")
  
  (event-task-event
   "The actual event to be delivered."))

;; event-loop.lisp
(setdocs
  (handler
   "Accesses the named handler from the event-loop, if such exists.

Handlers that do not have a NAME will be named by themselves.")
  
  (register-handler
   "Register the given handler so that it may receive events from the event-loop.

Registering the same handler twice will still result in it receiving
events only once, but will not cause an error. Registering a handler
might cause the event loop to be recompiled, which can be a costly
operation.

See RECOMPILE-EVENT-LOOP")
  
  (deregister-handler
   "Deregister the given handler so that it no longer receives events from the event-loop.

Deregistering an unregistered handler will not cause an error.
Deregistering a handler might cause the event loop to be recompiled,
which can be a costly operation.

See RECOMPILE-EVENT-LOOP")
  
  (sort-handlers
   "Sort the given list of handlers into the proper issuing sequence.

The returned list of handlers must be ordered in a way that the
BEFORE and AFTER lists of all handlers are met. If a dependency
cycle is detected, an error of type EVENT-LOOP-HANDLER-DEPENDENCY-CYCLE-ERROR
must be signalled.")
  
  (ensure-handlers-sorted
   "Ensures that the handlers are properly sorted on the event-loop

See SORTED-HANDLERS
See SORT-HANDLERS")
  
  (build-event-loop
   "Build a lambda form for the event loop delivery function.

The handlers must be in the properly sorted order as returned by
SORT-HANDLERS on the same EVENT-LOOP object as is used to call
this function.")
  
  (recompile-event-loop
   "Cause the event loop to be recompiled.

The event-loop's handlers must already be ready in their sorted
order before calling this function. This operation is potentially
very costly as it involves building a potentially very large
lambda and successively invoking COMPILE on it. With a large number
of registered handlers and a slow compiler, this might take up to
the order of seconds to run.

See BUILD-EVENT-LOOP
See ENSURE-HANDLERS-SORTED")
  
  ((event-loop type)
   "The base class for an event loop.

Implements a standard queued event loop that respects all other
constraints. Supports rudimentary optimisation of the event loop
handler filter tests to speed up delivery and generates a tight
event delivery function that should allow issuing events to
handlers very efficiently.

See QUEUED-EVENT-DELIVERY
See HANDLERS
See SORTED-HANDLERS
See LOCK")
  
  (handlers
   "An EQL hash-table of the registered handlers on the event-loop.
Be careful when modifying this table, as it is not synchronised.

See LOCK")
  
  (sorted-handlers
   "A list of the registered handlers in their properly sorted order.

This function might become temporarily out of sync with HANDLERS.

See SORT-HANDLERS
See ENSURE-HANDLERS-SORTED")
  
  (lock
   "A lock used to synchronise access to the event-loop slots.

See HANDLERS
See SORTED-HANDLERS
See DELIVERY-FUNCTION")
  
  ((*standard-event-loop* variable)
   "The default global instance of an event-loop.")
  
  ((do-issue)
   "Shorthand macro to allow more convenient issuing of events.

Supports one extra keyword argument that will not be passed along
to the MAKE-INSTANCE call: LOOP will instead denote the event-loop
to which the event is issued, defaulting to *STANDARD-EVENT-LOOP*

See *STANDARD-EVENT-LOOP*"))

;; event.lisp
(setdocs
  ((event-class type)
   "Metaclass for events.

Uses the EVENT-SLOT class for slots, to allow specification of im/mutable slots.

See EVENT-SLOT")
  
  ((event-slot type)
   "Base class for the event-slot classes.

Contains one extra field (:MUTABLE) that allows specification of
whether the slot is allowed to be mutated through a writer.
If the user attempts to specify a writer (or accessor) on the slot
while :MUTABLE NIL, a warning of type IMMUTABLE-EVENT-SLOT-HAS-WRITER
is signalled. If an attempt is made to write to a slot that has been
designated immutable, a continuable error of type IMMUTABLE-EVENT-SLOT-MODIFIED
is signalled. 

By default slots are immutable.

See EVENT-SLOT-MUTABLE")
  
  (event-slot-mutable
   "Accessor to whether the given EVENT-SLOT is mutable or not.

See EVENT-SLOT")
  
  ((event-direct-slot-definition type)
   "A standard-direct-slot-definition for an event-slot.")
  
  ((event-effective-slot-definition type)
   "A standard-effective-slot-definition for an event-slot.")
  
  ((event type)
   "Base class for all events.

Anything that would like to be processed by an event-delivery must
inherit from this class.

See ISSUE-TIME
See ORIGIN
See CANCELLED")
  
  (issue-time
   "The universal-time at which the event has been ISSUEd to an event-delivery.

See ISSUE.")
  
  (origin
   "Some description of an origin from where the event was issued.

See *ORIGIN*
See WITH-ORIGIN")
  
  (cancelled
   "Accessor to whether the event has been cancelled.

See CANCEL")
  
  ((define-event)
   "Shorthand convenience macro around DEFCLASS to define new event classes.

Pushes the EVENT-CLASS as :METACLASS and EVENT as direct-superclass
if it does not already appear somewhere as a transitive superclass.")
  
  (cancel
   "Cancels the event.

An event can be cancelled multiple times though the effect does not change.
Once an event has been cancelled it must not be handled by any further
handlers.

See CANCELLED")
  
  ((message-event type)
   "A simple event merely used to deliver message strings.

See EVENT
See MESSAGE")
  
  (message
   "The message of the message-event.")
  
  ((info-event type)
   "Delivers an informational message.

See MESSAGE-EVENT")
  
  ((warning-event type)
   "Delivers a warning message.

See MESSAGE-EVENT")
  
  ((error-event type)
   "Delivers an error message.

See MESSAGE-EVENT")
  
  ((payload-event type)
   "A simple event used to deliver some kind of data payload.

See EVENT
See PAYLOAD")
  
  (payload
   "The payload of to be delivered.")
  
  ((sequence-event type)
   "An event that is part of a sequence of events.

See EVENT
See INDEX
See MAX-INDEX")
  
  (index
   "The position of the event within the sequence.")

  (max-index
   "The maximum index of the sequence.

Might be NIL if the maximum length of the sequence is not yet known.")
  
  ((chunked-payload-event type)
   "A payload delivery event for which the payload has been cut up into chunks.

See PAYLOAD-EVENT
See SEQUENCE-EVENT")
  
  ((identified-event type)
   "An event that belongs to a certain identified group.

The identifier defaults to the event itself.

See EVENT
See IDENTIFIER")
  
  (identifier
   "The unique identifier of the group the event belongs to.")
  
  ((stream-event type)
   "Base event for stream events.

All stream events that belong to the same stream must have the same
identifier. Stream events are insignificant unless they are issued
after a STREAM-BEGIN-EVENT of the same identifier and before a
STREAM-END-EVENT of the same identifier.

See IDENTIFIED-EVENT")
  
  ((stream-begin-event type)
   "Stream event to signal the beginning of a stream.

All further stream events that belong to this stream must have the
same identifier as this. Events belonging to this stream may appear
until a STREAM-END-EVENT has been issued belonging to this stream.

See STREAM-EVENT")
  
  ((stream-payload-event type)
   "A payload event for a particular stream.

See PAYLOAD-EVENT
See STREAM-EVENT")
  
  ((stream-end-event type)
   "Stream event to signal the end of a stream.

See STREAM-EVENT"))

;; forward-class-definitions.lisp
(setdocs
  ((event-condition type)
   "Condition base class for conditions related to events.

See EVENT-CONDITION-EVENT")
  
  (event-condition-event
   "The event that the event condition is about.")
  
  ((immutable-event-slot-modified type)
   "An error that is signalled whenever an attempt is made to modify an immutable slot.

See EVENT-CONDITION
See EVENT-CONDITION-SLOT
See EVENT-CONDITION-VALUE")
  
  (event-condition-slot
   "The slot that the event condition is about.")
  
  (event-condition-value
   "The value that the event condition is about.")
  
  ((immutable-event-slot-has-writer type)
   "A warning that is signalled whenever an immutable slot is specified with a writer.

See EVENT-CONDITION
See EVENT-CONDITION-SLOT
See EVENT-CONDITION-WRITERS")
  
  (event-condition-writers
   "The writers that the event condition is about.")
  
  ((event-loop-condition type)
   "Condition base class for conditions related to event-loops.

See EVENT-LOOP-CONDITION-EVENT-LOOP")
  
  (event-loop-condition-event-loop
   "The event-loop that the event-loop condition is about.")
  
  ((event-loop-handler-dependency-cycle-error type)
   "An error that is signalled whenever a dependency cycle is detected within the handlers.

See EVENT-LOOP-CONDITION
See EVENT-LOOP-CONDITION-HANDLER")
  
  (event-loop-condition-handler
   "The handler that the event-loop condition is about.")
  
  ((handler-condition type)
   "Condition base class for conditions related to handlers.

See HANDLER-CONDITION-HANDLER")
  
  (handler-condition-handler
   "The handler that the handler condition is about.")
  
  ((handler-thread-stop-failed-warning type)
   "A warning that is signalled whenever a thread of a handler fails to stop.

See HANDLER-CONDITION
See HANDLER-CONDITION-THREAD")
  
  (handler-condition-thread
   "The thread that the handler condition is about."))

;; handler.lisp
(setdocs
  ((handler type)
   "Base class for all handlers to which events can be delivered.

See EVENT-DELIVERY
See NAME
See EVENT-TYPE
See FILTER
See BEFORE
See AFTER")
  
  (name
   "Returns a symbol describing the name of the object.")
  
  (event-type
   "The event-type that the handler accepts.")
  
  (filter
   "The specification of a filter to figure out which events the handler accepts.

The structure of a filter is as follows:

FILTER     ::= COMBINATOR | TEST
COMBINATOR ::= (AND TEST*) | (OR TEST*) | (NOT TEST)
TEST       ::= (function ARGUMENT*)
ARGUMENT   ::= fuzzy-slot-symbol | atom

fuzzy-slot-symbols denote the value of a slot on the event object.
The actual slot is figured out per FIND-CLASS-SLOT-FUZZY on the
EVENT-TYPE of the handler.

See FIND-CLASS-SLOT-FUZZY")
  
  (before
   "A list of handler names or categories before which this handler should be called.")
  
  (after
   "A list of handler names or categories after which this handler should be called.")
  
  ((parallel-handler type)
   "A handler that starts a new thread to handle each event that it receives through ISSUE.

Note that if you use this handler you must care for resource locking
yourself. This can potentially be very tricky and inefficient.

See HANDLER
See THREADS
See LOCK")
  
  (threads
   "The threads of the parallel-handler.")
  
  (lock
   "Handler lock to lock the access to the internal threads list.

See THREADS")
  
  ((queued-handler type)
   "A handler that uses an event queue and separate thread to process them.

This is the default handler class. Since there is only a single thread per
handler, resources on the handler itself should be safe to access. However,
you still have to tend after locking of external resources on your own as
they might be modified and accessed in any fashion.
Also note that since this is a single, queued handler that any event that
takes a very long time will block other events on the handler from being
processed. It will not however block the event-loop from which the events are
issued.

See HANDLER
See QUEUED-EVENT-DELIVERY")
  
  ((locally-blocking-handler type)
   "A handler that handles the event in the issuing thread and thus blocks it.

This is useful for handlers that need to modify the event they receive,
as then it is guaranteed that the modification is done before any further
handlers can be called with the event.

See HANDLER")
  
  ((globally-blocking-handler type)
   "A handler that globally blocks all issuing threads until it is done.

The difference to LOCALLY-BLOCKING-HANDLER is only apparent in the case
where multiple event loops issue to this same handler at the same time,
in which case both are blocked until their individual events are done
being processed.

See HANDLER
See QUEUED-EVENT-DELIVERY
See BLOCKING-EVENT-TASK")
  
  ((blocking-event-task type)
   "An event-task that blocks the issuing thread until it is done being processed in the foreign thread.

See EVENT-TASK
See SIMPLE-TASKS:BLOCKING-TASK")
  
  ((define-handler)
   "Define and register a new event handler on a particular event loop.

Defining a new handler this way will simply construct the handler object,
start it, and register it on the event loop. If a handler with the same
name already exists on the event loop, this old handler is stopped and
replaced by the new one.

ARGS must be a list of at least one value, which must be a symbol that is
bound to the event instance. The rest of the args are slots of the event,
bound by WITH-FUZZY-SLOT-BINDINGS.

The OPTIONS are passed to the make-instance call of the handler with the
exception of :LOOP, :CLASS, :FILTER, and :SELF. :LOOP specifies the loop
to which the event handler is registered. :CLASS specifies the name of
the class the handler is instantiated from. :FILTER specifies the filter
that specifies the accepted events. :SELF is a symbol bound to the handler
instance itself within the BODY. :CLASS defaults to QUEUED-HANDLER and 
:LOOP defaults to *STANDARD-EVENT-LOOP*.

The body of the handler function is within a WITH-ORIGIN environment, set
to the name of the handler.

See HANDLER
See REGISTER-HANDLER
See QUEUED-HANDLER
See *STANDARD-EVENT-LOOP*
See FILTER
See WITH-FUZZY-SLOT-BINDINGS
See WITH-ORIGIN")
  
  ((with-one-time-handler)
   "Constructs a handler that can only handle a single event before being deregistered again.

See DEFINE-HANDLER
See DEREGISTER-HANDLER"))

;; origin.lisp
(setdocs
  ((*origin* variable)
   "The variable designating the current origin.

The value of this might not be useful or significant for anything,
but if at all possible it should denote some kind of identifier for
the place where an event was signalled from.

See WITH-ORIGIN")
  
  ((with-origin)
   "Binds *ORIGIN* to a new value, which is by default figured out by HERE.

See *ORIGIN*
See HERE")
  
  (here
   "A macro that tries to figure out the most appropriate identifier for where this is."))

;; toolkit.lisp
(setdocs
  ((with-fuzzy-slot-bindings)
   "Establishes a binding context similar to WITH-SLOTS and WITH-ACCESSORS but using the most appropriate way to access a slot, and a fuzzy manner of identifying slots.

A slot is found if the designator matches by either slot-name or one
of the readers of the slot, through either an EQL or STRING= comparison.
If no appropriate slot can be found, an error is signalled.

If an identified slot is reachable through an accessor, then the
accessor is used. Otherwise SLOT-VALUE is used instead."))
