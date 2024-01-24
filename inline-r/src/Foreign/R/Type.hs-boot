module Foreign.R.Type where

data SEXPTYPE
    = Nil
    | Symbol
    | List
    | Closure
    | Env
    | Promise
    | Lang
    | Special
    | Builtin
    | SChar
    | Logical
    | SInt
    | Real
    | SComplex
    | SString
    | DotDotDot
    | Any
    | SVector
    | Expr
    | Bytecode
    | ExtPtr
    | WeakRef
    | Raw
    | S4
    | New
    | Free
    | Fun
