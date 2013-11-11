-- |
-- Copyright: (C) 2013 Amgen, Inc.
--

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds  #-}
{-# Language ExistentialQuantification #-}

module Data.Some where

data Some f = forall a. Some (f a)
