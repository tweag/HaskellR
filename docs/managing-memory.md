---
id: managing-memory
---

Managing memory
===============

One tricky aspect of bridging two languages with automatic memory
management such as R and Haskell is that we must be careful that the
garbage collectors (GC) of both languages see eye-to-eye. The embedded
R instance manages objects in its own heap, separate from the heap
that the GHC runtime manages. However, objects from one heap can
reference objects in the other heap and the other way around. This can
make garbage collection unsafe because neither GC's have a global view
of the object graph, only a partial view corresponding to the objects
in the heaps of each GC.

Memory protection
-----------------

Fortunately, R provides a mechanism to "protect" objects from garbage
collection until they are unprotected. We can use this mechanism to
prevent R's GC from deallocating objects that are still referenced by
at least one object in the Haskell heap.

One particular difficulty with protection is that one must not forget
to unprotect objects that have been protected, in order to avoid
memory leaks. `inline-r` uses "regions" for pinning an object in
memory and guaranteeing unprotection when the control flow exits
a region.

Memory regions
--------------

There is currently one global region for R values, but in the future
`inline-r` will have support for multiple (nested) regions. A region
is opened with the `runRegion` action, which creates a new region and
executes the given action in the scope of that region. All allocation
of R values during the course of the execution of the given action
will happen within this new region. All such values will remain
protected (i.e. pinned in memory) within the region. Once the action
returns, all allocated R values are marked as deallocatable garbage
all at once.

```Haskell
runRegion :: (forall s . R s a) -> IO a
```

Automatic memory management
---------------------------

Nested regions work well as a memory management discipline for simple
scenarios when the lifetime of an object can easily be made to fit
within nested scopes. For more complex scenarios, it is often much
easier to let memory be managed completely automatically, though at
the cost of some memory overhead and performance penalty. `inline-r`
provides a mechanism to attach finalizers to R values. This mechanism
piggybacks Haskell's GC to notify R's GC when it is safe to deallocate
a value.

```Haskell
automatic :: MonadR m => R.SEXP s a -> m (R.SEXP G a)
```

In this way, values may be deallocated far earlier than reaching the
end of a region: As soon as Haskell's GC recognizes a value to no
longer be reachable, and if the R GC agrees, the value is prone to be
deallocated. Because automatic values have a lifetime independent of
the scope of the current region, they are tagged with the global
region `G` (a type synonym for `GlobalRegion`).

For example:

```Haskell
do x <- [r| 1:1000 |]
   y <- [r| 2 |]
   return $ automatic [r| x_hs * y_hs |]
```

Automatic values can be mixed freely with other values.

Diagnosing memory problems
--------------------------

A good way to stress test whether R values are being protected
adequately is to turn on `gctorture`:

```Haskell
main = withEmbeddedR $ do
    [r| gctorture2(1, 0, TRUE) |]
    ...
```

This instructs R to run a GC sweep at every allocation, hence making
it much more likely to detect inadequately protected objects. It is
recommended to use a version of R that has been compiled with
`--enable-strict-barrier`.

See The Haddock generated documentation for the `Language.R.GC` module
for further details, and the gctorture for `gctorture`.
