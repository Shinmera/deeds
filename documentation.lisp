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
  ((cached-slots-class type)
   "A metaclass that caches all the direct slot instances available to it.

The cache is renewed on re/initialization of the class.

See CLASS-ALL-DIRECT-SLOTS")

  (class-all-direct-slots
   "Returns all direct slots of the given class, including those of its superclasses.

This can potentially be a very costly operation as it might have
to traverse the entire class hierarchy to find all slots.
If the class is a CACHED-SLOTS-CLASS, then the result will be
quick as it is cached. If it is not cached, it is computed by
COMPUTE-ALL-DIRECT-SLOTS.

See CACHED-SLOTS-CLASS
See COMPUTE-ALL-DIRECT-SLOTS")

  (compute-all-direct-slots
   "Computes all direct slots of the given class, including those of its superclasses.

This simply traverses the class hierarchy upwards, gathering all
direct-slots instances along the way. If one class along the way
is a CACHED-SLOTS-CLASS, the CLASS-ALL-DIRECT-SLOTS value is used
directly instead of traversing further. This does not apply for
the passed class.

See CACHED-SLOTS-CLASS"))

(setdocs
  ((command-event type)
   "Event used for commands

See DEFINE-COMMAND")

  ((define-command)
   "Define a command.

This is a combinging macro that does three things:
  . Define an event of NAME with the necessary fields from ARGS.
  . Define a function of NAME with the given ARGS that issues
    an instance of this newly defined event.
  . Define a handler of NAME on the NAME event with the given
    OPTIONS-AND-BODY.

A new option is available just for this macro with the name
:SUPERCLASSES, which allows you to specify the direct-superclasses
to use in the event definition.

The first argument in ARGS must be the name for the event as
required by DEFINE-HANDLER. The rest are arguments to the function
and simultaneously slots on the event class. In order to allow you
to specify slot options, arguments have the following structure:

ARGS       ::= SINGLE* [&optional DEFAULTING*] [&rest SINGLE*] [&key DEFAULTING*]
SINGLE     ::= symbol | (symbol SLOT-ARG*)
DEFAULTING ::= symbol | (symbol value SLOT-ARG*)
SLOT-ARG   ::= keyword value

The purpose of defining commands is to allow something akin to a
function definition that can be treated as such for most purposes
while still integrating it with the event system and allowing
extension through that."))

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
   "The actual event to be delivered.")

  ((blocking-event-task type)
   "A simple container to deliver events using the simple-tasks framework, blocking variant.

See EVENT-TASK"))

;; event-loop.lisp
(setdocs
  ((*standard-event-loop* variable)
   "The default global instance of a compiled-event-loop.")
  
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

  (deliver-event-directly
   "Performs a primitive, direct delivery of the event on the loop.

This means that handler filters are tested directly without any
possible room for optimisation.

See TEST-FILTER")
  
  (sort-handlers
   "Sort the given list of handlers into the proper issuing sequence.

The returned list of handlers must be ordered in a way that the
BEFORE and AFTER lists of all handlers are met. If a dependency
cycle is detected, an error of type EVENT-LOOP-HANDLER-DEPENDENCY-CYCLE-ERROR
must be signalled.

See SORTED-EVENT-LOOP")
  
  (ensure-handlers-sorted
   "Ensures that the handlers are properly sorted on the event-loop

See SORTED-HANDLERS
See SORT-HANDLERS
See SORTED-EVENT-LOOP")
  
  (build-event-loop
   "Build a lambda form for the event loop delivery function.

The handlers must be in the properly sorted order as returned by
SORT-HANDLERS on the same EVENT-LOOP object as is used to call
this function.

See COMPILED-EVENT-LOOP")
  
  (recompile-event-loop
   "Cause the event loop to be recompiled.

The event-loop's handlers must already be ready in their sorted
order before calling this function. This operation is potentially
very costly as it involves building a potentially very large
lambda and successively invoking COMPILE on it. With a large number
of registered handlers and a slow compiler, this might take up to
the order of seconds to run.

See BUILD-EVENT-LOOP
See ENSURE-HANDLERS-SORTED
See COMPILED-EVENT-LOOP")
  
  ((event-loop type)
   "The base class for an event loop.

This event loop is incredibly crude and should only serve as a basis
for either very specific scenarios or to build a new event-loop on
top of it. It does not sort the handlers properly nor optimise the
delivery in any way but simply tests and calls each handler in
whatever order it might find them in at the moment.

On the other hand, de/registering handlers will be very cheap on
this handler, as no recompilation or analysis is required.

See QUEUED-EVENT-DELIVERY
See HANDLERS
See EVENT-LOOP-LOCK
See SORTED-EVENT-LOOP
See COMPILED-EVENT-LOOP")
  
  (handlers
   "An EQL hash-table of the registered handlers on the event-loop.
Be careful when modifying this table, as it is not synchronised.

See EVENT-LOOP-LOCK")
  
  (event-loop-lock
   "A lock used to synchronise access to the event-loop slots.

See HANDLERS
See SORTED-HANDLERS
See DELIVERY-FUNCTION")

  (test-filter
   "Test the filter against the event.

Note that you must test whether the event is of applicable type as
required by the test yourself, since the test form does not contain
this information in itself.

The structure of a filter is as follows:

FILTER     ::= COMBINATOR | TEST
COMBINATOR ::= (AND TEST*) | (OR TEST*) | (NOT TEST)
TEST       ::= (function ARGUMENT*)
ARGUMENT   ::= fuzzy-slot-symbol | atom

fuzzy-slot-symbols denote the value of a slot on the event object.
The actual slot is figured out per FIND-CLASS-SLOT-FUZZY on the
EVENT-TYPE of the handler.

See FIND-CLASS-SLOT-FUZZY

See FILTER")
  
  ((do-issue)
   "Shorthand macro to allow more convenient issuing of events.

Supports one extra keyword argument that will not be passed along
to the MAKE-INSTANCE call: LOOP will instead denote the event-loop
to which the event is issued, defaulting to *STANDARD-EVENT-LOOP*

See *STANDARD-EVENT-LOOP*")

  ((sorted-event-loop type)
   "An event loop that respects the sorting order of handlers.

This event loop will always make sure the handlers are in proper
order to be called in, but does not perform any further optimisation.
As such, this handler should still be reasonably fast to de/register
handlers on, but will obviously suffer the deficiencies in issuing
due to the lack of optimisation.

See EVENT-LOOP
See SORTED-HANDLERS
See COMPILED-EVENT-LOOP")

  (sorted-handlers
   "A list of the registered handlers in their properly sorted order.

This function might become temporarily out of sync with HANDLERS.

See SORT-HANDLERS
See ENSURE-HANDLERS-SORTED
See SORTED-EVENT-LOOP")

  ((compiled-event-loop type)
   "An optimised event loop that compiles the issuing function for fast delivery.

Implements a standard queued event loop that respects all other
constraints. Supports rudimentary optimisation of the event loop
handler filter tests to speed up delivery and generates a tight
event delivery function that should allow issuing events to
handlers very efficiently.

See SORTED-EVENT-LOOP
See BUILD-EVENT-LOOP
See RECOMPILE-EVENT-LOOP"))

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

  (check-event-slots
   "Checks the given slot-forms to see if any immutable slots have writers.

If such a slot is found, a warning of type IMMUTABLE-EVENT-SLOT-HAS-WRITER
is signalled.")
  
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
Once an event has been cancelled it can only be handled by handlers that
have HANDLE-CANCELLED set to a non-NIL value.

See CANCELLED
See HANDLE-CANCELLED")

  ((blocking-event type)
   "A blocking event.

This event blocks on all handlers it passes through and blocks the issuing
thread until it is done being handled. This behaviour is sometimes desired,
especially in cases where remote communication is involved and sequential
execution on the issuing side must be ensured. This order cannot be
guaranteed with standard events, as while the events are issued in order
and handled in order by the same handler, they might change order between
different handlers.

See EVENT")
  
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
See AFTER
See HANDLE-CANCELLED")
  
  (name
   "Returns a symbol describing the name of the object.")
  
  (event-type
   "The event-type that the handler accepts.")
  
  (filter
   "The specification of a filter to figure out which events the handler accepts.

See TEST-FILTER")
  
  (before
   "A list of handler names or categories before which this handler should be called.")
  
  (after
   "A list of handler names or categories after which this handler should be called.")

  (loops
   "Accessor to all the loops the handler is registered on right now.")

  (handle-cancelled
   "Accessor to whether the handler will handle events even if they are marked as being cancelled.")
  
  ((parallel-handler type)
   "A handler that starts a new thread to handle each event that it receives through ISSUE.

Note that if you use this handler you must care for resource locking
yourself. This can potentially be very tricky and inefficient.

See HANDLER
See THREADS
See HANDLER-LOCK")
  
  (threads
   "The threads of the parallel-handler.")
  
  (handler-lock
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
See HANDLER-LOCK")

  (make-handler
   "Creates a new handler instance and takes care of registering it.

Creating a new handler this way will simply construct the handler object,
start it, and register it on the event loop. If a handler with the same
name already exists on the event loop, this old handler is stopped and
replaced by the new one.

Only the :LOOP and :CLASS options are not passed along to the
make-instance call, as they are used by make-handler itself.

See HANDLER
See REGISTER-HANDLER
See QUEUED-HANDLER
See *STANDARD-EVENT-LOOP*
See WITH-HANDLER
See DEFINE-HANDLER
See WITH-ONE-TIME-HANDLER")

  ((with-handler)
   "Convenient macro to construct a handler.

ARGS must be a list of at least one value, which must be a symbol that is
bound to the event instance. The rest of the args are slots of the event,
bound by WITH-FUZZY-SLOT-BINDINGS.

The actual body forms are composed into a lambda form which is passed
as the :DELIVERY-FUNCTION argument to MAKE-HANDLER alongside with
EVENT-TYPE. It is also wrapped in a WITH-ORIGIN environment where the
origin is set to the :NAME option.

See MAKE-HANDLER
See WITH-FUZZY-SLOT-BINDINGS
See WITH-ORIGIN")
  
  ((define-handler)
   "Define and register a new event handler on a particular event loop.

This simply expands to a WITH-HANDLER form with a few extra options
to make things easier. Most notably, NAME is transformed (quoted) into
the :NAME option. If the :SELF option is given, it names the symbol
to which the instance of the handler itself is bound, from which it is
accessible from within the handler body.

See WITH-HANDLER")

  ((one-time-handler type)
   "A handler that can only be called until it is successful.

If the delivery-function of the handler returns a non-NIL result, the
handler immediately deregisters itself from all its loops and stops
itself, thus only ever handling one \"correct\" event.

The return test is done to allow the user more sophisticated testing
of the event than is possible through the filter mechanism.

See QUEUED-HANDLER
See WITH-ONE-TIME-HANDLER")
  
  ((with-one-time-handler)
   "Constructs a handler that can only handle a single event before being deregistered again.

Defaults the class to ONE-TIME-HANDLER.

See WITH-HANDLER
See ONE-TIME-HANDLER
See DEREGISTER-HANDLER")

  ((condition-notify-handler type)
   "Handler that notifies a condition variable once it receives its event.
Saves the received event in its slot.

See ONE-TIME-HANDLER
See CONDITION-VARIABLE
See ISSUE-SYNCHRONIZER-LOCK
See RESPONSE-EVENT
See WITH-RESPONSE")

  (condition-variable
   "A condition variable accessor.")

  (issue-synchronizer-lock
   "Accessor to the lock used to synchronise the handler with the issuer.")

  (response-event
   "Accessor to the response event the handler captures.")

  ((with-response)
   "A macro to de-asynchronise waiting for a response to an event.

More specifically, this construct allows you to circumvent having to use
callbacks and instead allows the handling of an event that is in response
to an issued one in the same thread as if it all happened sequentially.

ISSUE ::= event-type | (event-type initarg*)
RESPONSE ::= event-type | (event-type [event-symbol slots*])

In detail the following happens when the block generated by this macro
is executed:
 . A handler is instantiated with the given response event-type and filter.
 . The handler is started up.
 . The handler's synchronizer lock is acquired.
 . The handler is registered on the given event-loop.
 . An event is issued with the event-type and initargs onto the event-loop.
 . Now the thread waits on the handler's condition variable.
 . Once the thread is reawakened before the timeout it binds the event
   from the handler and the specified slot variables, then proceeds to
   evaluate the body.
 . As a cleanup, the handler is deregistered and stopped.

The handler itself performs the following steps once it HANDLEs an event:
 . Acquire the one-time-handler exclusive lock.
 . Save the event on its own slot.
 . Acquire the synchronizer lock and immediately release it again to ensure
   that the issuing thread is waiting on the condition variable.
 . Notify the condition variable.

See DO-ISSUE
See REGISTER-HANDLER
See DEREGISTER-HANDLER
See WITH-FUZZY-SLOT-BINDINGS
See CONDITION-NOTIFY-HANDLER"))

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
  (class-all-direct-slots
   "Finds all direct-slot instances that this class will inherit.

More explicitly, it traverses the superclass tree and gathers all direct-slots it can find.")

  (find-slot-accessor
   "Attempts to find an accessor for the given slot.

This works by looping through all writers and seeing if there's a corresponding reader for it.
In the case of a (setf foo) writer it looks for a foo reader.")

  (find-class-slot-fuzzy
   "Attempts to find a slot on CLASS that matches SLOT-ISH

A slot is found if SLOT-ISH matches by either slot-name or one
of the readers of the slot through either an EQL or STRING= comparison.
If no appropriate slot can be found, NIL is returned instead.

See CLASS-ALL-DIRECT-SLOTS")

  (build-fuzzy-slot-accessor
   "Constructs an appropriate accessor form to read/write the given slot-ish.

FIND-CLASS-SLOT-FUZZY is used to find an appropriate slot.
If no appropriate slot can be found, an error is signalled.

FIND-SLOT-ACCESSOR is used to detect if an accessor can be
used. If no accessor is found, a SLOT-VALUE form is emitted
instead.

Returns the form and the slot instance.

See FIND-CLASS-SLOT-FUZZY
See FIND-SLOT-ACCESSOR")
  
  ((with-fuzzy-slot-bindings)
   "Establishes a binding context similar to WITH-SLOTS and WITH-ACCESSORS but using the most appropriate way to access a slot, and a fuzzy manner of identifying slots.

Each VAR can either be a list of NAME SLOT-ISH, or simply the SLOT-ISH which will be used as the name as well.

You must pass the class explicitly since we have to be able to analyse the slots of the class during compile time.

See BUILD-FUZZY-SLOT-ACCESSOR")

  (parse-into-kargs-and-body
   "Takes a list of forms and parses them into a list of kargs and body.

More specifically, it parses the following structure:

KARGS-AND-BODY ::= KARG* form*
KARG           ::= keyword form

This kind of structure is found on f.e. RESTART-CASE")

  (copy-hash-table
   "Copies the hash table.

This does not respect potential implementation-dependent hash-table properties.")

  (format-time
   "Formats UNIVERSAL-TIME into a datestring of the following format:
YYYY.MM.DD hh:mm:ss")

  (removef
   "Constructs a copy of LIST, removing the plist properties REMOVE-PROPERTIES.")

  (compile-lambda
   "Compiles the given LAMBDA form into a function object.

Attempts to muffle all warnings and notes during compilation."))
