---
title: How the R interpreter is embedded
id: how-r-interpreter-is-embedded
---

We embed an instance of the R interpreter using R's C API, documented
in the [Writing R extensions][R-exts] document. R only allows at most
one instance of the interpreter in any given process. Furthermore,
R's C API has the following important constraints:

* *it is not reentrant*, so only one thread should be accessing the
  R interpreter at any one time,
* the R interpreter *must* be running on the program's main thread.
  Otherwise you'll experience weird behaviour. See the
  [FAQ](faq.html).

[R-exts]: http://cran.r-project.org/doc/manuals/r-release/R-exts.html

### The threading model

In `inline-r`, single-threaded access is all but statically
guaranteed, thanks to the `R` monad. For flexibility, entering the
R monad is kept separate from initialization of the R instance via
`withEmbeddedR`, but `runRegion` should be called only once, near the
beginning of `main`. Arbitrary `IO` actions can be lifted into the `R`
monad, but not `forkIO` actions. `forkIO` forks `IO` actions, but
there is no way to interact with an R interpreter from the `IO` monad
if you call `runRegion` only once from the main thread. At any rate,
not if you keep to the API provided by the `Language.R.*` modules.

There is a backdoor if you really need it, called `unsafeRToIO`. But
as its name implies, calling it is at your own risks!

R insists that the interpreter should be run from the main thread.
Therefore, do not call `withEmbeddedR` from any other thread than the
main thread of the program. On some platforms, including OS X,
violating this assumption breaks all graphical event processing. On
all platforms, violating this assumption leads to strange call stack
and signaling issues.

The above constraint is a problem for H: GHCi insists on running the
read-eval-print loop on the main thread, while R insists on running
its own event processing loop in the main thread as well. Since there
is no way to mesh GHCi's loop with R's event loop, we have no other
option but to force the user to trigger event processing iterations
explicitly. This is done by calling the `Language.R.Event.refresh`
action. Other interactive interfaces may or may not have this
limitation.
