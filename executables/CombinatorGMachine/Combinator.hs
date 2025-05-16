module Combinator where

import Data.Text (Text)
import Prelude hiding (GT, LT)
import qualified Data.Text as Text
import qualified OP

-- The combinator language

-- Reference from MicroHaskell
data Combinator --                   Haskll            Allocate/Indirect
  = S  -- x y z     = x z (y x)      (<*>)                A
  | K  -- x y       = x              const, True, []      I
  | I  -- x         = x              id                   I
  | B  -- x y z     = x (y z)        (.)                  A
  | C  -- x y z     = (x z) y        flip                 A
  | A  -- x y       = y              False                I
  | U  -- x y       = y x            uncurrying
  | Z  -- x y z     = x y
  | P  -- x y z     = (z x) y        (,)                  A
  | R  -- x y z     = (y z) x                             A
  | O  -- x y z w   = (w x) y        (:)                  A
  | Y  -- x         = x (Y x)        fix
  | ADD
  | SUB
  | MUL
  | DIV
  | GT
  | GTE
  | LT
  | LTE
  | NOT
  | EQV
  | NEQ
  -- IO
  | PRINT
  | READ
  | BIND
  | PURE
  | ERROR
  deriving (Eq, Show)


arity :: Combinator -> Int
arity c =
  case c of
    S -> 3
    K -> 2
    I -> 1
    B -> 3
    C -> 3
    A -> 2
    U -> 2
    Z -> 3
    P -> 3
    R -> 3
    O -> 4
    Y -> 1
    ADD -> 2
    SUB -> 2
    MUL -> 2
    DIV -> 2
    GT -> 2
    GTE -> 2
    LT -> 2
    LTE -> 2
    NOT -> 1
    EQV -> 2
    NEQ -> 2
    PRINT -> 1
    READ -> 1
    BIND -> 1
    PURE -> 1
    ERROR -> 0


data Core
  = Var Text
  | Abs Text Core
  | App Core Core
  -- builtins
  | IntLit Int
  | Op OP.OP -- All operators are curried
  deriving (Eq, Show)


displayCore :: Core -> String
displayCore core =
  case core of
    Var n -> Text.unpack n
    Abs x body -> "\\" ++ Text.unpack x ++ "." ++ displayCore body ++ ""
    App a b -> "(" ++ displayCore a ++ " @ " ++ displayCore b ++ ")"
    IntLit n -> show n
    Op op -> show op


table :: [(Text, Combinator)]
table =
  [ ("S", S)
  , ("K", K)
  , ("I", I)
  , ("B", B)
  , ("C", C)
  , ("A", A)
  , ("U", U)
  , ("Z", Z)
  , ("P", P)
  , ("R", R)
  , ("O", O)
  , ("Y", Y)
  , ("ADD", ADD)
  , ("SUB", SUB)
  , ("MUL", MUL)
  , ("DIV", DIV)
  , ("GT", GT)
  , ("GTE", GTE)
  , ("LT", LT)
  , ("LTE", LTE)
  , ("NOT", NOT)
  , ("EQV", EQV)
  , ("NEQ", NEQ)
  , ("PRINT", PRINT)
  , ("READ", READ)
  , ("BIND", BIND)
  , ("PURE", PURE)
  , ("ERROR", ERROR)
  ]
