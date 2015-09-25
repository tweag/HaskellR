---
title: Evaluating R expressions in H
id: evaluating-r-expressions
---

At the H interactive prompt, the most convenient way to interact with
R is through quasiquotation. `r` is a quasiquoter that constructs an
R expression, ships it off to R and has the expression evaluated by R,
yielding a value.

    H> [r| 1 |]
    0x00007f355520ab38

We do not normally manipulate R values directly, instead leaving them
alone for R to manage. In Haskell, we manipulate *handles* to
R values, rather than the values themselves directly. Handles have
types of the form `SEXP s a`, which is actually a type synonym for
a pointer type, so values of type `SEXP s a` are not terribly
interesting in their own right: they are just memory addresses, as
seen above. However, one can use `H.print` to ask R to show us the
value pointed to by a handle:

    H> H.printQuote [r| 1 + 1 |]
    [1] 2
    H> H.printQuote [r| x <- 1; x + 2 |]
    [1] 3
    H> H.printQuote [r| x |]
    [1] 1

In the following, we will loosely identify *handles to R values* and
*R values*, since the distinction between the two is seldom relevant.

The `r` quasiquoter hides much of the heavy lifting of building
expressions ourselves, allowing us to conveniently use R syntax to
denote R expressions. The next sections document some advanced uses of
quasiquotes. But for now, note that `r` is not the only quasiquoter
and one is free to implement [new quasiquoters][quasiquotation] if
needed. One such alternative quasiquoter is `rexp`, also defined in H,
which acts in much the same way as `r`, except that it returns
R expressions unevaluated:

    H> H.printQuote [rexp| 1 + 1 |]
    expression(1+1)

Because quasiquoters ask R itself to parse expressions, at quasiquote
expansion time, H itself need not implement its own R parser. This
means that the entirety of R’s syntax is supported, and that it is
possible to take advantage of any future additions to the R syntax for
free in H, without any extra effort.

[quasiquotation]: http://dl.acm.org/citation.cfm?id=1291211

In H, graphical facilities are readily available. For example:

    H> [r| plot(cars) |]

NOTE: if you resize the graphics window, you'll notice that this
window might not be repainted. In fact, the window might not even
close properly. The reason for this is because since H is a thin
wrapper around GHCi, and
[there is currently no way](differences-repl-source.html) to mesh
GHCi's read-eval-print loop with R's event loop, events must be
processed manually, by calling `H.refresh` at the prompt. IHaskell
does not have this limitation.
