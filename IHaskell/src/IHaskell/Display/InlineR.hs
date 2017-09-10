-- |
-- Copyright: 2015 (C) Tweag I/O Limited

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module IHaskell.Display.InlineR
  ( r
  , rprint
  , rgraph
  , Language.R.Instance.runRegion
  ) where

import           Control.Applicative
import           Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as Char
import           Data.Monoid
import           H.Prelude.Interactive as H -- we use provide instances to IO Monad
import           IHaskell.Display
import           Language.Haskell.TH.Quote
import           Language.R.Instance
import           System.IO (hClose)
import           System.IO.Temp (withSystemTempFile)
import qualified Text.Blaze.Html5 as BH
import qualified Text.Blaze.Html5.Attributes as BH
import Prelude -- Silence AMP warning

rprint :: QuasiQuoter
rprint = QuasiQuoter { quoteExp = \s -> [| do H.p $(quoteExp r s) |] }

rgraph :: QuasiQuoter
rgraph = QuasiQuoter { quoteExp = \s ->
    [| withSystemTempFile "ihaskell-inline-r-.png" $ \path h -> do
          hClose h
          _ <- [r| png(filename=path_hs, width=480, height=480, bg="white"); |]
          H.p $(quoteExp r s)
          _ <- [r| dev.off() |]
          encoded <- Base64.encode <$> BS.readFile path
          when (BS.null encoded) $
            fail "No graphical output."
          display $
            BH.img BH.!
              BH.src (BH.unsafeByteStringValue
                        (Char.pack "data:image/png;base64," <> encoded))
     |] }
