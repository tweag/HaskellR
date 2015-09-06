---
id: casts-and-coercions
---
## Casts and coercions

Type indexing `SEXP`s makes it possible to precisely characterize the
set of values that a function can accept as argument or return as
a result, but this only works well when the forms of R values are
known *a priori*, which is not always the case. In particular, the
type of the result of a call to the `r` quasiquoter is always
`SomeSEXP`, meaning that the form of the result is not statically
known. If the result needs to be passed to a function with a precise
signature, say `SEXP s R.Real -> SEXP s R.Logical`, then one needs to
either discover the form of the result, by first performing pattern
matching on the result before passing it to the function:

```Haskell
f :: SEXP s R.Real -> SEXP s R.Logical

g = do SomeSEXP x <- [r| 1 + 1 |]
       case x of
         (hexp -> R.Int v) -> return (f x)
         _ -> error "Not an int."
```

But pattern matching in this manner can be verbose, and sometimes the
user knows more than the type checker does. In the example above, we
know that `[r| 1 + 1 |]` will always return a real. We can use *casts*
or *coercions* to inform the type checker of this:

```Haskell
f :: SEXP s R.Real -> SEXP s R.Logical

g = do x <- [r| 1 + 1 |]
       return $ f (R.Int `R.cast` x)
```

A *cast* introduces a dynamic form check at runtime to verify that the
form of the result was indeed of the specified type. This dynamic type
check was a (very small) cost. If the user is extra sure about the
form, she may use *coercions* to avoid even the dynamic check, when
the situation warrants it (say in tight loops). This is done with

```Haskell
unsafeCoerce :: SEXP s a -> SEXP s b
```

This function is highly unsafe - it is H's equivalent of Haskell's
`System.IO.Unsafe.unsafeCoerce`. It is a trapdoor that can break type
safety: if the form of the argument happens to not match the expected
form at runtime then a segfault may result, or worse, silent memory
corruption.
