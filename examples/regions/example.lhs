This file describes a regions and provides a number of motivating examples.
It should be used to test that we don't have a breaking changes, and region
are still have required semantics. This is a literate haskell file, so it's
possible to compile and run program.

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE QuasiQuotes #-}
> module Main where


Required imports for H program:

> import H.Prelude
> import Language.R.QQ
> import Control.Monad.R
> import Data.Int
> import qualified Foreign.R as R

Import few unsafe bit:

> import qualified Language.R.Literal.Unsafe as Unsafe
> import qualified Foreign.R.Internal        as Unsafe
> import qualified Foreign.R.Internal
> import qualified Control.Monad.R.Unsafe    as Unsafe

Start a program:

> main = runR defaultConfig $ do
>   [r| gctorture(TRUE) |]

---------------------------------------------------------------------------------
--  Low level code                                                             --
---------------------------------------------------------------------------------

The initial program is run in the main one big region (R Monad)
in order to show how things can work without regions we will
run an a bit of unsafe low level code, we use `Unsafe.unsafeIOToR` to run IO 
code:

>   Unsafe.unsafeIOToR $ do
>     putStrLn "hello world"
>     x <- Unsafe.unsafeMkSEXP (42::Int32)

Variable 'x' is not protected in the R runtime, so it can be removed
on any allocation in R. Using @gctorture@ we force removing all unprotected
values on every allocation.

>     Prelude.putStrLn "should be: Int"
>     Prelude.print (Unsafe.typeOf x)

We see that it's an @Int@ now. Now let's force GC and check output
type.

>     Prelude.putStrLn "should be: Free"
>     gc
>     Prelude.print (Unsafe.typeOf x)

We see that value is @Free@ now, this means that the value was removed
during the R GC. This is not wanted behavior, and there are a few
ways to protect a variable. First one is to use a 'protect' method
provided by R. If 'protect' is used then value is stored in 'R' protects
stack (array) and is not removed during GC. To mark value as avaliable
to be freed one can call 'Foreign.R.Internal.unprotect' method, and
give a number of objects to remove from the stack.

Note: there is a way to remove a value ('unprotectPtr') not only from the top,
but it is not effective and should not be used in user code.

Here is an example of code:

>     Prelude.putStrLn "Create a SEXP variable"
>     x1 <- Unsafe.unsafeMkSEXP (42::Int32)
>     Prelude.putStrLn "Protect a SEXP variable using Foreign.R.Internal.protect"
>     _  <- Foreign.R.Internal.protect x1
>     gc
>     Prelude.putStrLn "Value type should be: Int"
>     Prelude.print (Unsafe.typeOf x)
>     Prelude.putStrLn "Unprotect variable"
>     Unsafe.unprotect 1
>     gc
>     Prelude.putStrLn "should be: Free"
>     Prelude.print (Unsafe.typeOf x)

Using protect and unprotect is error prone, in order to make things easier
to track R imposed an 'stack balance invariant', there are a list of
functions that checks a protection stack size and once value becomes different
before and after a function this means that there is a problem in user code.

To make a life on an H user easier we have introduced 'withProtected' method
that uses bracket like function to protect value when it's used:

>     Prelude.putStrLn "Protect value using: withProtected call."
>     Unsafe.withProtected (Unsafe.unsafeMkSEXP (42::Int32)) $ \x2 -> do
>       gc
>       Prelude.putStrLn "Should be: Int"
>       Prelude.print (Unsafe.typeOf x2)

This gives an coincise and relatively easy way to write a safe code (and this method
s used internally and in QQ-generated code), however there are 2 problems with
this approach:

  1. the code becomes not easy to understand once a number of values becomes large

  2. The code is not safe as value can still escape 'withProtected' block

>     Prelude.putStrLn "Example of leaking withProtected block."
>     x3 <- Unsafe.withProtected (Unsafe.unsafeMkSEXP (42::Int32)) $ \x3 -> do
>             gc
>             return x3
>     gc
>     Prelude.putStrLn "Value is not protected, so it's type should be FREE."
>     Prelude.print (Unsafe.typeOf x3)


The latter becomes a real problem due to the fact that some computations may leak
a protection block due to Haskell laziness.


As a result we introduced a safe layer on the top of the lower layer, that is
based on the Monadic Regions proposed by Oleg Kiselov. Our approach is differ
from the original one because propersies of the R runtime do not allow to translate
original solution on our case.

---------------------------------------------------------------------------------
--  Regions                                                                    --
---------------------------------------------------------------------------------

At the hightest layer Monadic Region is a block of code where user can allocate
various resources and all those resources are guaranteed to be protected until
the end of the region, and no resource can leak the region. We can think of the
H-s regions like a Reader monad that has an information about number of protected
resources and performs unprotection at the end of the block, plus some type-level
trickery.

Resources are the objects that are allocated and used in haskell as a toplevel
variables. Resources can be created using following methods:

  1. objects that are allocated by 'mkSEXP' call;
  2. objects that are allocated by 'unhexp' call;
  3. objects that are allocated as a result of 'QQ'-generated code.
  4. objects that are explicitly protected by the 'protect' call.

All intermediate values or values in foreign functions are not concidered as 
resources and can be freed, this doesn't break safety guarantees.

>   Unsafe.unsafeIOToR $ Prelude.putStrLn "Region examples"
>   runRegion $ do
>     Unsafe.unsafeIOToR $ Prelude.putStrLn "Allocate variable in the region"
>     x <- mkSEXP (42::Int32)
>     gcr
>     Unsafe.unsafeIOToR $ Prelude.putStrLn "Should be Int"
>     Unsafe.unsafeIOToR $ Prelude.print (R.typeOf x)
>     return ()

Call to R is done in a one big region, this means that all top-level variables
will not be freed until the end of the program. In order to make write a code
that does unallocations one can use 'runRegion' call. 'runRegion' starts a region
block and frees all resources that are allocated inside a region at exit.

Variables in a region:


Variables that are protected inside a region should have an 's' typevariable
in them, or example 'SEXP':

newtype SEXP s a = SEXP (Unsafe.SEXP a)

This allow to protect a variable from escaping the region scope. Currenlty R
provides following protected variables:

  1. SEXP s a    -- S-Expression
  2. SomeSEXP s  -- S-Expression of an unknown type
  3. Data.Vector.SEXP.Region.Vector s a -- Mutable vector view on the S-Expression

User can introduce his own protected variable types.

In order to protect a variable one can use `protect` method

protect :: Protect s a => a -> R s (ProtectElt s a)

This function takes an object that can be protected inside a block and returns
a new protected object. This allowes to have a same interface for all datatypes.
It's safe to use an unprotected version of the object in the region however
user should not do it, and expose API for working with such objects in safe modules.

Protect typeclass has a default implementation for all values that do not
need special care, i.e. where protected value is equal to unprotected.

There is a limitation for protection, currently protection is done using 'protect'
method and object counter, so it's not possible to protect a variables that
are not representially equal to 'SEXP a' and have additional unprotection actions
rather then 'unprotect'. [TODO] if user need such behaviour he should create
a weak pointer for an object and associate an unallocation action with it.

Another big difference comparing to original regions is an unprotection procedure.
Because protection is lead by a linear stack we can protect values inside a parent
region and do a fancy things like duplicating variables to the parent regions
without complication of the procedue.

We have introduced an unprotection procedure, this procedure prepares a variable
to for usafe outside a region, this procedure to 2 things:

  1. Verifies that there is no reference to the variables protected inside a
  region in thunks. This is done by full evaluation of the object
  (requires NFData constraint).

  2. Changes a type to the one that explicitly shows that value is not protected
  anymore, so user is responced for it's protection.

NB. unprotect doesn't do any actual unprotection, unprotection is run only at the
end of the block, it only prepares a variable for be safely returned from the region.

`runRegion` function takes care of automatic unprotection of the resulting variables.

runRegion
  :: Unprotect a => (forall s. R s a) -> R s' (UnprotectElt a)

There is a special type 'UnsafeValue a' that is used to hide an unprotected
variable to used knows that he have to protect it before a use.

Here is a small example.

>   Unsafe.unsafeIOToR $ Prelude.putStrLn "Region example:"
>   x5 <- runRegion $ do
>     Unsafe.unsafeIOToR $ Prelude.putStrLn "Create a variable"
>     x <- mkSEXP (42::Int32)
>     Unsafe.unsafeIOToR $ Prelude.putStrLn "Unprotect a variable"
>     y <- unprotect x  -- really we don't need to have it :/
>     Unsafe.unsafeIOToR $ Prelude.putStrLn "Return unprotected variable"
>     return y
>   Unsafe.unsafeIOToR $ Prelude.putStrLn "Unsafe use of variable"
>   unsafeUseValue x5 (\z -> Unsafe.unsafeIOToR $ Prelude.print (Unsafe.typeOf z))
>   Unsafe.unsafeIOToR $ Prelude.putStrLn "Explicit protection"
>   x6 <- protect x5
>   gcr
>   Unsafe.unsafeIOToR $ Prelude.print (R.typeOf x6)


Passing values to the region
----------------------------

Passing a values can be one easily. If you need to use a value from
a parent region inside a child, then you can use 'protect =<< unprotect' that
is safe to use.

[TODO] There should be a problems but I can't find an example

>   x7 <- mkSEXP (42::Int32)
>   runRegion $ testVariablePass x7



TODO: region with witness. 
difference with Oleg's regions and ours.

>   x6 <- newRegion $ \(SubRegion wt) -> do
>      wt $ mkSEXP (42::Int32)
>   unsafeUseValue x6 (\z -> Unsafe.unsafeIOToR $ Prelude.print (Unsafe.typeOf z))

TODO: nested regions.

TODO: non injectivity of a UnprotectElt.

NB: there is no way to deallocate a structure before a end of a region (for now)


Function with region inside.

> testVariablePass :: R.SEXP g a -> R s ()
> testVariablePass x = runRegion $ do
>     t <- protect =<< unprotect x
>     Unsafe.unsafeIOToR $ Prelude.print (R.typeOf x)

Helper functions.

> gc :: IO ()
> gc = do _ <- Unsafe.allocList 1
>         putStrLn "Perform GC."
>         return ()

> gcr :: R s ()
> gcr = Unsafe.unsafeIOToR gc


