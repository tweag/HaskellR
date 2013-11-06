-- This module is intended to be imported qualified.

-- Record field names are not intended to be exported. They exist merely to take
-- advantage of the record punning syntax extension and as documentation of what
-- the field does.

-- Use explicit UNPACK pragmas rather than -funbox-strict-fields in order to get
-- warnings if a field is not unpacked when we expect it to.

-- TODO Use GADT to encode invariants documented in "R Internals Guide".

import Data.Int (Int32)
import Data.Vector (Vector)
import Data.ByteString (ByteString)
import Data.Complex (Complex)

data HExp
  -- Primitive types. The field names match those of <RInternals.h>.
  = Nil
  | Symbol
    { pname    :: !HExp
    , value    :: !HExp
    , internal :: !HExp
    }
  | List
    { carval :: !HExp
    , cdrval :: !HExp
    , tagval :: !HExp
    }
  | Env
    { frame   :: !HExp
    , enclos  :: !HExp
    , hashtab :: !HExp
    }
  | Closure
    { formals :: !HExp
    , body    :: !HExp
    , env     :: !HExp
    }
  | Promise
    { value :: !HExp
    , expr  :: !HExp
    , env   :: !HExp
    }

  -- Derived types. These types don't have their own 'struct' declaration in
  -- <RInternals.h>.
  | Lang
    { function :: !HExp
    , args     :: !HExp
    }
  | Special
    { offset :: {-# UNPACK #-} !Int32
    }
  | Builtin
    { offset :: {-# UNPACK #-} !Int32
    }
  | Char
    { length     :: {-# UNPACK #-} !Int32
    , truelength :: {-# UNPACK #-} !Int32
    , chars      :: {-# UNPACK #-} !ByteString
    }
  | Int
    { length     :: {-# UNPACK #-} !Int32
    , truelength :: {-# UNPACK #-} !Int32
    , ints       :: {-# UNPACK #-} !(Vector Int32)
    }
  | Real
    { length     :: {-# UNPACK #-} !Int32
    , truelength :: {-# UNPACK #-} !Int32
    , reals      :: {-# UNPACK #-} !(Vector Double)
    }
  | Complex
    { length     :: {-# UNPACK #-} !Int32
    , truelength :: {-# UNPACK #-} !Int32
    , complexes  :: {-# UNPACK #-} !(Vector (Complex Double))
    }
  | String
    { length     :: {-# UNPACK #-} !Int32
    , truelength :: {-# UNPACK #-} !Int32
    , strings    :: {-# UNPACK #-} !(Vector ByteString)
    }
  | DotDotDot
    { args :: HExp
    }
  | Any
  | Vector
    { length     :: {-# UNPACK #-} !Int32
    , truelength :: {-# UNPACK #-} !Int32
    , elements   :: {-# UNPACK #-} !(Vector HExp)
    }
  | Expr
    { length     :: {-# UNPACK #-} !Int32
    , truelength :: {-# UNPACK #-} !Int32
    , exprs      :: {-# UNPACK #-} !(Vector HExp)
    }
  | Bytecode -- XXX
  | ExtPtr
    { pointer :: !HExp
    , protectionValue :: !HExp
    , tag :: !HExp
    }
  | WeakRef
    { length     :: {-# UNPACK #-} !Int32
    , truelength :: {-# UNPACK #-} !Int32
    , key        :: !HExp
    , value      :: !HExp
    , finalizer  :: !HExp
    , next       :: !HExp
    }
  | Raw
    { length     :: {-# UNPACK #-} !Int32
    , truelength :: {-# UNPACK #-} !Int32
    , bytes      :: {-# UNPACK #-} !ByteString
    }
  | S4
    { tag :: !HExp
    }
