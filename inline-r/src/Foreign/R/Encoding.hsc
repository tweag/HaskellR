-- | Character encodings.

#include <Rinternals.h>

module Foreign.R.Encoding where

-- | Content encoding.
data CEType
  = CE_Native
  | CE_UTF8
  | CE_Latin1
  | CE_Bytes
  | CE_Symbol
  | CE_Any
  deriving (Eq, Show)

instance Enum CEType where
  fromEnum CE_Native = #const CE_NATIVE
  fromEnum CE_UTF8   = #const CE_UTF8
  fromEnum CE_Latin1 = #const CE_LATIN1
  fromEnum CE_Bytes  = #const CE_BYTES
  fromEnum CE_Symbol = #const CE_SYMBOL
  fromEnum CE_Any    = #const CE_ANY
  toEnum i = case i of
    (#const CE_NATIVE) -> CE_Native
    (#const CE_UTF8)   -> CE_UTF8
    (#const CE_LATIN1) -> CE_Latin1
    (#const CE_BYTES)  -> CE_Bytes
    (#const CE_SYMBOL) -> CE_Symbol
    (#const CE_ANY)    -> CE_Any
    _ -> error "CEType.fromEnum: unknown tag"
