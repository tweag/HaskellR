---
id: implementation-of-quasiquoters
---
Implementation of quasiquoters
==============================

Given an R expression, represented as a `String`, a quasiquote expands
to a function whose body is the given expression, and the arguments
are any antiquotations appearing in the expression.

For this function to be available at runtime, we must construct it
somehow. There are two possible approaches. Historically, `inline-r`
parsed a quasiquotation once at compile-time, then used code
generation to generate a Haskell expression that recreates the
resulting AST at runtime. It is much simpler, however, to express the
`SEXP` value as the result of parsing a string constructed at compile
time. Parsing the function expression is delegated to R at runtime.
Parsing the same expression over and over during runtime can be
onerous when in the middle of a tight loop. But since parsing is
morally a pure function, we can wrap the call to R's `parse()`
function `unsafePerformIO` to make it eligible for let-floating. If
the `parse()` call is floated all the way to the top, then computing
its value will be shared for the lifetime of the program.

For example, `[r| 1 + 1 |]` expands to something morally equivalent
to the following:

```Haskell
let sx = unsafePerformIO (parse "function(){ 1 + 1 }")
in eval (apply sx [])
```

Use the `-ddump-simpl` GHC option to see what a quasiquotation truly
expands to.
