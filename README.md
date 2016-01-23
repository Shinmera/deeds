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
This section documents how the particular standard implementation of the various components works. Some to all of this can be changed by subclassing the existing implementations and overriding the behaviour to suit your needs.

### Events
In order to allow for any form of optimisation on the event filtering in the event loop, we need to enforce restrictions on what can happen to an event. The most straightforward restriction is to require immutability of the event slots. However, in some cases it is very much desirable to be able to mutate the slots -- as an example if you'd like to filter the text in a message event. As such, the most appropriate solution is to allow manual overriding of the immutability on a per-slot basis. To achieve this, a custom metaclass `event-class` along with custom slot classes `event-direct-slot-definition` `event-effective-slot-definition` have been created that tend to this behaviour.

The slot classes have an additional field accessible through `event-slot-mutable` and specifiable through `:mutable`, which dictates whether the slot is mutable or not. If it is immutable but a writer has been specified on the slot anyway, an `immutable-event-slot-has-writer` warning is signalled upon class initialisation to warn the user of potentially dangerous behaviour. Similarly, if an attempt is made to actually perform a modification on an immutable slot during any time but the `event`'s `initialize-instance` time, a continuable error of type `immutable-event-slot-modified` is signalled. This way it is still possible to perform modifications on immutable slots if absolutely necessary, while the accidental case is prevented.

In order to make defining events easier, a simple wrapper around `defclass` is provided called `define-event`. A bunch of standard events are supplied to make the most common event actions easier: `message-event`, `info-event`, `warning-event`, `error-event`, `payload-event`, `sequence-event`, `chunked-payload-event`, `identified-event`, `stream-event`, `stream-begin-event`, `stream-payload-event`, and `stream-end-event`.

### Handlers
The primary distinguishing feature of a handler is the question of how it actually invokes the delivery function when an event is `issue`d to the handler from a loop. The most primitive behaviour is to simply call the function directly. This is implemented by the `locally-blocking-handler`, which in effect will block the issuing loop, but may still be invoked in parallel if multiple loops are involved. The next idea is to spawn a new thread for each event that is issued. This behaviour is implemented by the `parallel-handler`. However, spawning lots of threads will quickly lead to situations where the threads interleave, and as such it is important that the delivery function takes care to lock and synchronise, which can be taxing on the user. Thus the next idea is to queue events and handle them in a separate thread. This avoids most contention and thrashing issues while still getting the benefit of unblocking the loop. This behaviour is implemented by the `queued-handler`. Finally there is a variant of the `locally-blocking-handler` that blocks all loops that have issued an event until their respective event is done. This is implemented by the `globally-blocking-handler`.

Potentially much more complicated schemes that act differently depending on the issuing loop or the event to be processed could be conceived, but such things are too tied to the actual use-case at hand, and as such are left up to the user to implement. Writing such a handler should not prove very difficult, it is merely a matter of subclassing `handler` and implementing the appropriate behaviour in `issue` and `handle`. The handler can then be used by supplying it to `define-handler` by the `:class` option.

### Event Loops
Finally the most complicated piece of machinery is the event loop itself. In order to understand the workings, first the lifecycle of an event must be analysed. From a user's perspective this would go as follows: an event instance is created and issued onto a loop. It is then handled by the appropriate handlers in the order as specified by their dependency restrictions. The most primitive way to handle this would be to build a dependency graph of the handlers and traverse them in their topological order, only invoking them when their filter applies to the event.

However, this can become severely limiting when the amount of events and handlers becomes significant. The first optimisation is to separate the loop out into a dedicated thread with an event queue that buffers events. This unblocks the issuing thread and ensures that nothing is held up waiting. The next obvious improvement is to cache the dependency graph walking by generating the proper order of handlers upon handler registration. Finally we can realise that the event handler filters will share a lot of common tests which can be potentially rather costly if repeated too often. It would be good if those could be cached and bundled together to avoid unnecessary retesting.

The default implementation `compiled-event-loop` in Deeds solves these in the following manner: the unblocking of the issuing thread is achieved through [simple-tasks](http://shinmera.github.io/simple-tasks/)' `queued-runner`, which already provides the exact mechanism we need. The dependency graph is calculated through [Tarjan's algorithm](https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm) and cached as a list. It is recalculated every time a new handler is registered by `sort-handlers`. Finally, to make the event loop efficient it is treated as a `compile`d function that is built dynamically whenever necessary through `build-event-loop`. The mechanism of this is rather complicated.

First, in order to optimise and cache tests the loop is separated into three "phases". The first phase establishes lexical bindings for all tests and performs the type tests on the event object. The second phase performs the actual tests on the event as extracted from the handlers' filters if they are applicable under the current event class. Finally, the handlers are called in their `sorted-handlers` order if their filter passes. The bulk of this behaviour is done by `extract-tests` which uses `filter-tests` to extract all tests from a filter. Tests are simply compared through `equal` to see if they are the "same". This is a very conservative comparison as several types of tests would return the same result even if their forms are not `equal` such as for commutative tests. `compile-test` is used to replace the variable references within a test with event slot accessors as discovered by `build-fuzzy-slot-accessor`. Finally `replace-tests` is used to replace the tests in a filter with the corresponding variables that are outputted to cache the tests. This is then all packed together into a `lambda` form that can then be `compile`d into a function object.

The optimisations done here are not optimal however. For example, no effort is done to group `when` forms together in the outputted form when possible. This could save on branching and repeat tests in the resulting form. Similarly, no effort is done to recognise whether the required type tests of the handlers are actually strictly necessary or could be weakened to allow further grouping and conflating of tests. There are potentially many more possible improvements that could be done to make the event loop function even tighter, however the algorithms necessary to achieve this would quickly explode in complexity. For now we settle on this rather primitive approach as being sufficient.

However, you can of course extend the system on your own by subclassing `event-loop`/`sorted-event-loop`/`compiled-event-loop` and implementing the proper methods such as `build-event-loop`, `sort-handlers`, `register-handler`, and `deregister-handler`.
