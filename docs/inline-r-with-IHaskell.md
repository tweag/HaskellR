---
id: inline-r-with-IHaskell
title: Using inline-r with IHaskell
---

It's possible to use `inline-r` with IHaskell notebook in order to get
benefits of R without introducing much costs for interporability
between languages.

## Preface

Everything could work more or less like [ihaskell-rlangqq](https://hackage.haskell.org/package/ihaskell-rlangqq).
The main difference is that H uses embedded R. This means that you'll get

  * no costs associated with values serialization and deserialization and
    passing via pipes. You still may need to pay for converting some haskell values to R values,
    but this will be binary convertion in the same address space.

  * Ability to pass Haskell callbacks.

There are currently a few drawbacks:

  * `rlangqq` is enabled by default so names will be in conflict, so
    inline-r should be imported qualified;

  * There is no automatic graphics support, so you need to ask R to output
    plots to files explicitly.

Both of the issues can fixed once `inline-r` gains wider community adoption.

## Installation and use

In order to install H with IHaskell you need to install IHaskell on the host,
and `ihaskell-inline-r` wrapper.


We demonstrate how to use `inline-r` through a simple notebook.

Start by loading the required extensions

```haskell
:ext TemplateHaskell QuasiQuotes
```


All the instances needed for the interactive run of `inline-r` are
preloaded for you (see
[details](/HaskellR/docs/differences-in-h-and-repl.html)) but you may
want to import `H.Prelude` anyway to get access at functions such as
`H.printQuote`.

```haskell
import qualified H.Prelude as H
```

Finally, we just initialise `H` with `initializeEmbeddedR`. Note that
this command is idempotent so you can run it as many times
as you like. If you're only using quasi-quoters then this
initialisation is not strictly necessary but if you plan on using R
functions more directly from the `inline-r` library, you need it.

```haskell
initializeEmbeddedR defaultConfig
```

In order to use `inline-r` in your notebook you need to use `h`
quasiquote. As you may have noticed `H` itself uses `r` quasiquote,
but `r` is is already widely used in **ihaskell** by **rlangqq**. As
there is no sane way to hide packages, we decided to use `h`:

```haskell
 [h| 1 + 1|]
    0x00007fe054a90798
```


As per
[HaskellR manual](/HaskellR/docs/evaluating-r-expressions.html), this
an address of the result. What if we want to see the value of the
result instead of its address? You can use the `hDisp` quasi-quoter
which outputs the result in a human-readable form. The downside is
that it's not as composable as the `h` quasi-quoter as it always has
the type `IO ()` so it can't be used as part of a different
expression. An alternative to `hDisp` quasi-quoter is using
`H.printQuote`.

```haskell
 [hDisp| 1+1|]
    [1] 2

 H.printQuote [h| 1 + 1|]
    [1] 2
```

Please note that if you're using the `console` version of
**ihaskell**, there is a small chance that the result will get printed
twice. Rest assured that the evaluation only happens once. The bug is
not present if you use the in-browser version of
**ihaskell**/**ipython** (`notebook`).


Having plots in R is not as straightforward as one would like. In
order to output a plot to a file one first needs to set up a
[plot device](https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/png.html)
(one of `pdf`, `bmp`, `png`, etc functions), plot all the data you
want and finally use `dev.off()` once you're done plotting to flush
the data into file. All this means is that you might end up having to
write something like

```haskell
_ <- [h| png("Rplots/5-%03d.png"); plot(c(1:10),mapply(sin,c(1:10))); dev.off()|]
```

Note that **R** will happily overwrite your existing files: the "%03d"
suffix is only used for page number of the plot. To make the common
case easier, and to cut down on boilerplate that the user has to
write, we provide the `hPlot` quasi-quoter:

```haskell
[hPlot| plot(c(1:10), mapply(cos,c(1:10)))|]
```

Roughly, it follows the following steps:

  1. initializes `png` device;
  2. outputs plots to file "Rplots/auto-%i.png" where `i` is the highest index to be found there;
  3. flushes data with `dev.off()`;
  4. shows plot in the `notebook`.

Result:
<div>
<img
src="data:image/png;base64,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">
</div>

This is more or less everything you need to know to get started though
a few words about resource handling instance for `IO` are in order.
The instance protects the top-level values indefinitely meaning that R
can't free the allocated objects. While this makes sense for top-level
values, values inside quasi-quotes behave in the same way, something
we probably don't want. If you run `[h| sum(allocate10GbVector) |]`,
the 10Gb vector will not be freed as the user might expect. The
solution to this is to use regions which you can read about in the
[HaskellR manual](/HaskellR/docs/managing-memory.html):

```haskell
import H.Prelude.Interactive
import qualified Foreign.R as R
import qualified Foreign.R.Type as R
runRegion $ fmap (fromSEXP . R.cast R.SReal) [h| 1 + 1|] :: IO Double

    2.0
```

<div>
<style>/* Custom IHaskell CSS. */

/* Styles used for the Hoogle display in the pager */
.hoogle-doc {
    display: block;
    padding-bottom: 1.3em;
    padding-left: 0.4em;
}
.hoogle-code {
    display: block;
    font-family: monospace;
    white-space: pre;
}
.hoogle-text {
    display: block;
}
.hoogle-name {
    color: green;
    font-weight: bold;
}
.hoogle-head {
    font-weight: bold;
}
.hoogle-sub {
    display: block;
    margin-left: 0.4em;
}
.hoogle-package {
    font-weight: bold;
    font-style: italic;
}
.hoogle-module {
    font-weight: bold;
}
.hoogle-class {
    font-weight: bold;
}

/* Styles used for basic displays */
.get-type {
    color: green;
    font-weight: bold;
    font-family: monospace;
    display: block;
    white-space: pre-wrap;
}

.show-type {
    color: green;
    font-weight: bold;
    font-family: monospace;
    margin-left: 1em;
}

.mono {
    font-family: monospace;
    display: block;
}

.err-msg {
    color: red;
    font-style: italic;
    font-family: monospace;
    white-space: pre;
    display: block;
}

#unshowable {
    color: red;
    font-weight: bold;
}

.err-msg.in.collapse {
  padding-top: 0.7em;
}

/* Code that will get highlighted before it is highlighted */
.highlight-code {
    white-space: pre;
    font-family: monospace;
}

/* Hlint styles */
.suggestion-warning {
    font-weight: bold;
    color: rgb(200, 130, 0);
}
.suggestion-error {
    font-weight: bold;
    color: red;
}
.suggestion-name {
    font-weight: bold;
}
</style>
</div>
