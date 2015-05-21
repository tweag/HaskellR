---
id: how-r-interpreter-is-embedded
permalink: how-r-interpreter-is-embedded.html
---

How the R interpreter is embedded
=================================

The following threads materialize when H is used:

 The R thread
  : The only thread that can execute calls of the R API.

 The GUI timer thread
  : A thread that periodically produces requests to handle GUI events
    in the R thread. The GUI timer thread is killed as soon as the R
    thread dies.

In the public interface of H, operations which require interaction with
the embedded R interpreter communicate with the R thread in a synchronous
fashion. Exceptions produced in the R thread are rethrown to the caller.
Operations in the R monad work like this, and similar operations are
available on the IO monad when using GHCi.

Internal operations may require to be evaluated in the R thread. The
following function is provided in the module `Language.R.Instance`
for this sake:

```Haskell
unsafeRunInRThread :: IO a -> IO a
```

The /unsafe/ prefix means that no verification is made that the R
thread is running.

The GUI timer thread is necessary whenever any of the graphic
capabilities of R is used. Otherwise, when displaying windows, the
user interface would not respond to mouse clicks or keystrokes.
