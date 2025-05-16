module OP (OP(..)) where

-- language agnostic operators

data OP
  = Add
  | Sub
  | Mul
  | Div
  | Gt
  | Gte
  | Lt
  | Lte
  | Not
  | Eqv
  | Neq
  deriving (Eq, Show)
