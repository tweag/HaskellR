---
id: r-values-and-side-effects
---

### R values and side effects

As explained previously, values of type `SEXP s a` are in fact pointers
to structures stored on the R heap, an area of memory managed by the
R interpreter. As such, one must take heed of the following two
observations when using a pure function such as `hexp` to dereference
such pointers.

First, just like regular Haskell values are allocated on the GHC heap
and managed by the GHC runtime, values pointed to by `SEXP`'s are
allocated on the R heap, managed by the R runtime. Therefore, the
lifetime of a SEXP value cannot extrude the lifetime of the R runtime
itself, just as any Haskell value becomes garbage once the GHC runtime
is terminated. That is, never invoke `hexp` on a value outside of the
dynamic scope of `runR`. This isn't a problem in practice, because
`runR` should be invoked only once, in the `main` function of the
program`, just as the GHC runtime is initialized and terminated only
once, at the entry point of the binary (i.e., C's main() function).

The second observation is that the `hexp` view function does pointer
dereferencing, which is a side-effect, yet it claims to be a pure
function. The pointer that is being dereferenced is the argument to
the function, of type `SEXP s a`. The reason dereferencing a pointer is
considered an effect is because its value depends on the state of the
global memory at the time when it occurs. This is because a pointer
identifies a memory location, called a *cell*, whose content can be
mutated.

Why then, does `hexp` claim to be pure? The reason is that H assumes
and encourages a restricted mode of use of R that rules out mutation
of the content of any cell. In the absence of mutation, dereferencing
a pointer will always yield the same value, so no longer needs to be
classified as an effect. The restricted mode of use in question bans
any use of side-effects that break referential transparency. So-called
*benign* side-effects, extremely common in R, do not compromise
referential transparency and so are allowed.

Is such an assumption reasonable? After all, many R functions use
mutation and other side effects internally. However, it is also the
case that R uses *value semantics*, not *reference semantics*. That
is, including when passing arguments to functions, variables are
always bound to values, not to references to those values. Therefore,
given *e.g.* a global binding `x <- c(1, 2, 3)`, no call to any
function `f(x)` will alter the *value* of x, because all side-effects
of functions act on *copies* of `x` (the formal parameter of the
function doesn't share a reference). For example:

```R
> x <- c(1,2,3)
> f <- function(y) y[1] <- 42
> f(x)
> x
[1] 1 2 3
```

Furthermore, in R, closures capture copies of their environment, so
that even the following preserves the value of `x`:

```R
> x <- c(1,2,3)
> f <- function() x[1] <- 42
> f()
> x
[1] 1 2 3
```

The upshot is that due to its value semantics, R effectively limits
the scope of any mutation effects to the lexical scope of the
function. Therefore, any function whose only side-effect is mutation
is safe to call from a pure context.

Conversely, evaluating any `SEXP` in a pure context in Haskell is
*unsafe* in the presence of mutation of the global environment. For
example,

```Haskell
f :: SEXP s a -> (R s SomeSEXP,HExp a)
f x = let h = hexp x
         in ([r| x_hs <- 'hello' |], h)
```

The value of the expression `snd (f x)` depends on whether it is evaluated
before or after evaluating the monadic computation `fst (f x)`.

*Note:* H merely encourages, but has of course no way of *enforcing*
a principled use of R that keeps to benign side-effects, because owing
to the dynamic typing of R code, there is no precise static analysis
that can detect bad side-effects.
