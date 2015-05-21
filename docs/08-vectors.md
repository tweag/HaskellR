---
id: vectors
permalink: vectors.html
---

## Vectors

Most data items in R are vectors, e.g. integers, reals, characters,
etc. H supports constructing and manipulating R vectors entirely in
Haskell, without invoking the R interpreter, and using the same API as
the de facto standard
[vector](http://hackage.haskell.org/package/vector) package.
Conversely, any data that is stored as an R vector rather than some
other vector type can be fed to R functions without any prior
conversion or copying. Considering that the memory layout of an
R vector is practically as efficient as any other unboxed
representation, programs that interact with the R interpreter
frequently should consider using R vectors as a representation by
default.

Please refer to the Haddock generated documentation of the
`Data.Vector.SEXP` and `Data.Vector.SEXP.Mutable` modules for a full
reference on the vector API supported by H.

