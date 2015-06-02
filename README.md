GitHub WebHook Handler
----------------------

This package provides a generic implementation of a GitHub WebHook Handler,
abstracted over the differences of various Haskell web frameworks (Snap, Yesod,
Scotty etc).

You probably want to use one of the bindings for the web framework you're
using. If no implementation exists yet you can write it yourself, see further
below how to get started with that.


### Existing Bindings

 - Snap: [github-webhook-handler-snap][snap]


### How to write your own binding

The main data structure is `Handler m`, which provides the `runHandler` function
with configuration and framework-specific implementation of various functions
which the handler needs.

The type parameter `m` is the monad which the handler uses. Almost each Haskell
web framework has its own monad, for example snap has `Snap`, yesod has `Handler`
etc. If in doubt, just use `IO`.

Construct a `Handler m` for your web framework and then call `runHandler`. That
function attempts to handle the request. It will always invoke the action
callback (`hAction`). It is up to you to send a proper response back to the
GitHub server.

Note that `runHandler` doesn't catch any exceptions. It is up to you to ensure
that those are caught and processed.

The [snap binding][snap] can serve as an example.


[snap]: https://github.com/wereHamster/github-webhook-handler-snap
