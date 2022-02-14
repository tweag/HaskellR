---
id: r-monad
---
The R monad
===========

All expressions like

~~~
[r| ... |]   :: MonadR m => m (SEXP s b)
H.print sexp :: MonadR m => m ()
H.eval sexp  :: MonadR m => m (SEXP s b)
~~~

are computations in a monad instantiating `MonadR`.

~~~ haskell
class (Applicative m, MonadIO m) => MonadR m where
  io :: IO a -> m a
~~~

These monads ensure that:

 1. the R interpreter is initialized;
 1. resources managed by the R interpreter do not extrude its
    lifetime;
 1. constraints concerning on which system thread the R interpreter
    can run are respected.

There are two instances of `MonadR`: `IO` and `R`. Which instance one
uses depends on the context: the `IO` monad in an interactive session,
the `R` monad in compiled code.

Interactive frontends such as H and IHaskell bring the `IO` instance
into scope. In source files, you should not use this instance and
instead use the `R` monad, for better static guarantees. Functions are
provided in the `Language.R.Instance` module to initialize R and to
run `R` computations in the `IO` monad:

~~~ haskell
withEmbeddedR :: Config -> IO a -> IO a
runRegion     :: (forall s . R s a) -> IO a
io            :: IO a -> R s a
unsafeRToIO   :: R s a -> IO a
~~~

The `IO` monad is used in interactive sessions as a mere convenience.
It allows evaluating expressions without the need to wrap every
command at the prompt with a function to run the `R` monad. The `IO`
monad is in theory not as safe as the `R` monad, because it does not
statically guarantee that R has been properly initialized, but in the
context of an interactive session this is superfluous as the
`H --interactive` command takes care of initialization at startup.
