---
title: Catching runtime errors
id: catching-runtime-errors
---

Evaluating R expressions may result in runtime errors. All errors are
wrapped in the `Foreign.R.Error.RError` exception that carries the
error message.

    H> (H.printQuote [r| plot() |])
         `catch` (\(H.RError msg) -> putStrLn msg)
    Error in xy.coords(x, y, xlabel, ylabel, log) :
      argument "x" is missing, with no default
