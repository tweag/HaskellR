-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Vectors that can be passed to and from R with no copying at all. These
-- vectors are a special instance of "Data.Vector.Storable" where the memory is
-- allocated from the R heap, and in such a way that they can be converted to
-- a 'SEXP' through simple pointer arithmetic (see 'toSEXP').
--
-- The main difference between 'Data.Vector.SEXP.Vector' and 
-- 'Data.Vector.Storable.Vector' is that the former uses header-prefixed data layout.
-- This means that no additional pointer jump is needed to reach the vector data.
-- The trade-off is that all slicing operations are O(N) instead of O(1) and there
-- is no mutable instance of the SEXP vector.
--
-- Note that since 'unstream' relies on slicing operations, it will still be an O(N)
-- operation but it will copy vector data twice unlike most vector implementations.
--
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Data.Vector.SEXP.Region
  (
    -- * Mutable vectors of 'SEXP' types
    MVector(..)
  , ElemRep
  -- * Accessors
  -- ** Length information
  , length
  , null
  -- * Construction
  -- ** Initialisation
  , new
  , unsafeNew
  , replicate
--  , replicateM
  , clone
  -- ** Restricting memory usage
  , clear
  -- * Accessing individual elements
  , read
  , write
  , swap
  , unsafeRead
  , unsafeWrite
  , unsafeSwap
  -- * Modifying vectors
  -- ** Filling and copying
  , set
  , copy
  , move
  , unsafeCopy
  , unsafeMove
  -- * Convertion
  , toUnsafe
  , fromUnsafe
  , toUnsafeI
  , fromUnsafeI
--  -- * SEXP specific.
--  , fromSEXP
--  , toSEXP
--  , unsafeToStorable
--  , fromStorable
  ) where

import           Foreign.R
import           Data.Vector.SEXP.Base
import qualified Data.Vector.SEXP.Mutable as Unsafe
import qualified Data.Vector.SEXP as UnsafeI
import           Control.Monad.R (R, unsafeIOToR)

import           Prelude (Bool, Int, ($))
import           Control.Monad
import           Control.Category

newtype MVector s ty a = MVector { unMVector :: SEXP s ty } 

toUnsafe :: (Unsafe.SexpVector ty a) => MVector s ty a -> Unsafe.IOVector ty a
toUnsafe (MVector s) = Unsafe.MVector (unSEXP s)

fromUnsafe :: (Unsafe.SexpVector ty a) => Unsafe.IOVector ty a -> MVector s ty a
fromUnsafe (Unsafe.MVector s) = MVector (SEXP s)

toUnsafeI :: (Unsafe.SexpVector ty a) => MVector s ty a -> UnsafeI.Vector ty a
toUnsafeI (MVector s) = UnsafeI.Vector (unSEXP s)

fromUnsafeI :: (Unsafe.SexpVector ty a) => UnsafeI.Vector ty a -> MVector s ty a
fromUnsafeI (UnsafeI.Vector s) = MVector (SEXP s)

---------------------------------------------------------------------------------
-- Reexports
---------------------------------------------------------------------------------
length :: (Unsafe.SexpVector ty a) => MVector s ty a -> R s Int
length = return . Unsafe.length . toUnsafe 

null :: (Unsafe.SexpVector ty a) => MVector s ty a -> R s Bool
null = return . Unsafe.null . toUnsafe 

new :: (Unsafe.SexpVector ty a) => Int -> R s (MVector s ty a)
new = fmap fromUnsafe . unsafeIOToR . Unsafe.new 

unsafeNew :: (Unsafe.SexpVector ty a) => Int -> R s (MVector s ty a)
unsafeNew = fmap fromUnsafe . unsafeIOToR . Unsafe.unsafeNew

replicate :: (Unsafe.SexpVector ty a) => Int -> a -> R s (MVector s ty a)
replicate n v = fmap fromUnsafe $ unsafeIOToR $ Unsafe.replicate n v

clone :: (Unsafe.SexpVector ty a) => MVector s ty a -> R s (MVector s ty a)
clone = fmap fromUnsafe . unsafeIOToR . Unsafe.clone . toUnsafe

clear :: (Unsafe.SexpVector ty a) => MVector s ty a -> R s ()
clear = unsafeIOToR . Unsafe.clear . toUnsafe

read :: (Unsafe.SexpVector ty a) => MVector s ty a -> Int -> R s a
read v i = unsafeIOToR $ Unsafe.read (toUnsafe v) i

write :: (Unsafe.SexpVector ty a) => MVector s ty a -> Int -> a -> R s ()
write w i v = unsafeIOToR $ Unsafe.write (toUnsafe w) i v

swap :: (Unsafe.SexpVector ty a) => MVector s ty a -> Int -> Int -> R s ()
swap w i j = unsafeIOToR $ Unsafe.swap (toUnsafe w) i j

unsafeRead :: (Unsafe.SexpVector ty a) => MVector s ty a -> Int -> R s a
unsafeRead w i = unsafeIOToR $ Unsafe.unsafeRead (toUnsafe w) i

unsafeWrite :: (Unsafe.SexpVector ty a) => MVector s ty a -> Int -> a -> R s ()
unsafeWrite w i a = unsafeIOToR $ Unsafe.unsafeWrite (toUnsafe w) i a

unsafeSwap :: Unsafe.SexpVector ty a => MVector s ty a -> Int -> Int -> R s ()
unsafeSwap w i j = unsafeIOToR $ Unsafe.unsafeSwap (toUnsafe w) i j

set :: Unsafe.SexpVector ty a => MVector s ty a -> a -> R s ()
set w v = unsafeIOToR $ Unsafe.set (toUnsafe w) v

copy :: Unsafe.SexpVector ty a => MVector s ty a -> MVector s ty a -> R s ()
copy w v = unsafeIOToR $ Unsafe.copy (toUnsafe w) (toUnsafe v)

move :: Unsafe.SexpVector ty a => MVector s ty a -> MVector s ty a -> R s ()
move w v = unsafeIOToR $ Unsafe.move (toUnsafe w) (toUnsafe v)

unsafeCopy :: Unsafe.SexpVector ty a => MVector s ty a -> MVector s ty a -> R s ()
unsafeCopy w v = unsafeIOToR $ Unsafe.unsafeCopy (toUnsafe w) (toUnsafe v)

unsafeMove :: Unsafe.SexpVector ty a => MVector s ty a -> MVector s ty a -> R s ()
unsafeMove w v = unsafeIOToR $ Unsafe.unsafeMove (toUnsafe w) (toUnsafe v)
