-- |
-- Copyright: (C) 2013 Amgen, Inc.
--

module H where

import Data.Text (Text)

-- | Input: the text of an R script.
--
--   Output: the text of a GHCi script.
translate :: Text -> Text
translate = id
