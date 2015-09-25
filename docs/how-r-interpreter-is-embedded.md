---
id: how-r-interpreter-is-embedded
---

## How the R interpreter is embedded

We embed an instance of the R interpreter using R's C API, documented
in the [Writing R extensions][R-exts] document. R only allows at most
one instance of the interpreter in any given process. Furthermore, R's
C API is not reentrant: only one thread should be accessing the
R interpreter at any one time.

[R-exts]: http://cran.r-project.org/doc/manuals/r-release/R-exts.html

### The threading model

In `inline-r`, single-threaded access is all but statically
guaranteed, thanks to the `R` monad. Arbitrary `IO` actions can be
lifted into the `R` monad, including `forkIO` actions. But since
`forkIO` can only fork `IO` actions, so long as `withEmbeddedR` is
called only once at the beginning of the `main` function, there is no
way to fork actions that interact with the R interpreter in separate
threads. At any rate, no such action can be defined using only
`Language.R.*` modules.

You can fork as many threads as you like. It's that just all threads
except the main thread will be `IO` threads, not `R` threads.

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
