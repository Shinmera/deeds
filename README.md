## About Deeds
Deeds is an Extensible Event Delivery System. It allows for efficient event delivery to multiple handlers with a complex event filtering system.

There are three main components to this system: events, loops, and handlers. Events are certain kinds of messages or data payloads that are to be sent out and processed by other parts in the system. Loops handle the distribution of the events, and handlers then finally do the processing of an event.

There can be multiple independent loops in any application and handlers can be registered on any number of loops simultaneously. However, each event should only be sent out once and only on one loop. If it is necessary to issue an event multiple times or onto multiple loops, it needs to be copied.

The distribution to handlers from the loop follows a strict ordering scheme that is specified on the handlers. Each handler can dictate a list of `before` and `after` elements that precisely lay out their position in the issuing sequence. Additionally, handlers can specify a filter and event class that limit the type and kind of events that it will receive.

The loop compiles an efficient, tailored function to issue events to handlers and perform the filtering. This means that even with a significant amount of handlers, issuing an event is still going to be very fast.

Finally there are different types of handlers to give the user more control over the context in which the event handling happens and whether the loop should be blocked during the handler's execution.

## How To
Load Deeds through Quicklisp or ASDF:

    (ql:quickload :deeds)

By default the system will set up and launch a standard event loop on `*standard-event-loop*`. You can then add handlers by using `define-handler`:

    (deeds:define-handler (info-handler deeds:info-event) (event message)
      (format T "~&INFO: ~a~%" message))
    
    (deeds:define-handler (smiling-handler deeds:info-event) (event message)
      :filter (search ":)" message)
      (format T "~&YAY: ~a~%" message))

Now let's issue some events and see what happens:

    (deeds:do-issue deeds:info-event :message "Hello!")
    (deeds:do-issue deeds:info-event :message "Hey there :)")
    (deeds:do-issue deeds:warning-event :message "Help! It's burning!")

By default events have fields for the issue-time, the origin, and whether they've been cancelled. So let's play around with that.

    (deeds:define-handler (simple-handler deeds:event) (event)
      (format T "~&Found event: ~a~%" event))

    (deeds:define-handler (pre-handler deeds:event) (event)
      :before '(:main)
      :class 'deeds:locally-blocking-handler
      (deeds:cancel event)
      (format T "~&Cancelling!~%"))

The first handler simply prints everything it finds. The second one is special in two ways: first it orders itself before every other normal handler by ordering itself before the main category. Second, it uses a different handler class. The default class is the `queued-handler` which uses a singular queue in a different thread to process events. This handler on the other hand will not use any threads and instead run directly in the event loop, thus locally blocking it until the handler is done. This allows us to make sure that the event gets cancelled before it reaches anything else:

    (deeds:do-issue deeds:event)

As you can see, only the pre-handler is called. If you want to try it out, remove the `cancel` call and recompile the pre-handler. Then the event should reach both handlers again. Alternatively you can add the `:handle-cancelled T` option to the simple-handler to override the default behaviour.

## Internals

### Events

### Handlers

### Event Loops
