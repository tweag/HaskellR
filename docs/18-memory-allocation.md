---
id: memory-allocation
permalink: memory-allocation.html
---

Memory allocation
=================

H allocates memory in two separate heaps, managed by two separate
garbage collectors: the R garbage collector and the one that is part
of the GHC runtime. Either heap can in general contain pointers to
values in the other heap, yet neither garbage collector is aware of
what is going on in the heap that it does not manage. As a result, one
must be very careful when allocating memory to ensure that one GC does
not cut the grass under the feet of the other, e.g. by deallocating
a value pointed to from the other heap. H provides a wide list of
features and mechanisms for safe and easy memory management.

Description of garbage collection systems
-----------------------------------------

### Haskell garbage collection system

Haskell has a concurrent, stop-the-world, copying generational GC.
This means that all evaluation will be stopped while GC is performed.
And Haskell data structures can`t be used transparently in foreign
code, since the GC may move them. However, Haskell does support pinned
memory, which is not relocatable.

Another option for using Haskell structures in foreign runtime is
through the use of the
[StablePtr](http://hackage.haskell.org/package/base-4.6.0.1/docs/Foreign-StablePtr.html).
A stable pointer is a reference to a Haskell expression that is
guaranteed not to be affected by garbage collection, i.e., it will
neither be deallocated nor will the value of the stable pointer itself
change during GC (ordinary references may be relocated during garbage
collection).

To check objects being used, the Haskell GC traverses objects starting
from the top level (root) objects and relocates usable objects, so no
additional protection is required. For protection of foreign memory
Haskell uses Foreign.Ptr that contains a finalizer callback that is
invoked when the object is no longer used in Haskell.

### R garbage collection system.

R uses a single threaded, stop-the-world, non copying generational GC.

Newly created structures in C that are not yet connected to the object
graph can be protected from the GC using the following mechanisms:

  * `PROTECT`, and `PROTECT_PTR` macros for protection
  * `REPROTECT` for protection of the updated object
  * `UNPROTECT` and `UNPROTECT_PTR` for removing protection.

Every object that is reachable from a protected variable is also
protected.

Basic strategy of H
-------------------

To keep objects safe from garbage collection, H implements the
following strategy. Every object that may be reachable from R is
allocated in the R heap so that R can perform deallocation and
protection of values. For the programmer, H provides additional
functionality that can help to keep values safe.

### Low level functions

Low level functions may be used inside the IO or R monad to protect
variables in H.

  * `protect` -- synonym for PROTECT macros

  * `unprotect` -- synonym for UNPROTECT_PTR macros

  * `withProtected` -- bracket function that protects variable and
      unprotects it at the end. Usage example:

    ```haskell
    H.eval =<< withProtected (mkString "H.Home") R.lang1
    ```

    Here String SEXP "H.Home" is protected until it will be a part of
    LangSEXP, created by `R.lang1`. Then `R.lang1` becomes
    unprotected, which is fine as it immediately enters evaluation
    where it can't be freed until end of the execution. If the
    resulting SEXP doesn't enter evaluation you'll need to protect it
    in the toplevel (see [Toplevel
    expressions](#toplevel-expressions)).

Using these low-level functions directly is not normally necessary:
the regions mechanism is a thin layer on top of them that offers
a number of static guarantees. See `Language.R.Instance` and
`Language.R.GC`.

### Allocating SEXP

SEXP allocation can be done in the following ways:

  1. Using the low-level `alloc*` functions. Such variables are not
  protected.

  2. With `unhexp`. See [SEXP introspection and
  construction](#sexp-introspectio-and-construction). Such variables
  require explicit protection.

  3. Using quasi-quotation. Such variables are automatically
  protected, since the quasiquoter generates code that uses
  `withProtected`, so only toplevel expression should be protected.

### Function parameters

Most parameter types cannot cross memory boundaries or are explicitly
copied when they are passed to and from functions. However it possible
to coerce some types with zero-copying:

  * `SEXP s a` -> `SEXP s b`
  * `SEXP s (Vector Real)` -> Vector.Storable Double
  * `SEXP s (Vector Int)`  -> Vector.Storable Int
  * `SEXP s (Vector Logical)` -> Vector.Storable Bool
  * `SEXP s (Vector Char)`    -> Vector.Storable Word8
  * `SEXP s (Vector Char)`    -> Vector.Storable ByteString

### Automatic `SEXP`'s

Some `SEXP` values have automatic memory memory management. See
`Language.R.GC`. This automatic memory management comes at
a performance cost, and does not scale to more than a few hundred
automatically managed values.

To protect Haskell variables from the GC, a global registry is used,
which is a global mutable variable that maintains a list of variables
that are used by R code. As is the case for much of the rest of the
R library, the code for manipulating this global registry is non
reentrant. So great care should be taken in a concurrent setting,
whose performance, moreover, could well be affected by the
synchronization point that is the global registry.

Currently, this global registry is the one already provided by the
R interpreter (documented [here][R-exts-gc]), in the
form of the `R_PreserveObject()`/`R_ReleaseObject()` pair of
functions. Internally, these functions keep track of protected values
in a linked list, which `R_ReleaseObject()` scans linearly.

[R-ints]: http://cran.r-project.org/doc/manuals/R-ints.html
[R-exts]: http://cran.r-project.org/doc/manuals/r-release/R-exts.html
[R-exts-gc]: http://cran.r-project.org/doc/manuals/r-release/R-exts.html#Garbage-Collection

