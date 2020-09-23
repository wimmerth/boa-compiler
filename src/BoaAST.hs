-- Abstract syntax definitions for Boa. Do not modify anything!

module BoaAST where

data Value =
    NoneVal
  | TrueVal | FalseVal
  | IntVal Int
  | StringVal String
  | ListVal [Value]
  deriving (Eq, Show, Read)

data Exp =
    Const Value
  | Var VName
  | Oper Op Exp Exp
  | Not Exp
  | Call FName [Exp]
  | List [Exp]
  | Compr Exp [CClause]
  deriving (Eq, Show, Read)

type VName = String
type FName = String

data Op = Plus | Minus | Times | Div | Mod | Eq | Less | Greater | In
  deriving (Eq, Show, Read)

data CClause =
    CCFor VName Exp
  | CCIf Exp
  deriving (Eq, Show, Read)

type Program = [Stmt]  

data Stmt =
    SDef VName Exp
  | SExp Exp
  deriving (Eq, Show, Read)
