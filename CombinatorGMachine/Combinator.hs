module Combinator where
import Data.Text (Text)
import Data.String (IsString)
import Prelude hiding (GT, LT)

-- The combinator language

data Combinator
  = S  -- x y z     = x z (y x)      (<*>)
  | K  -- x y       = y              const, False, []
  | I  -- x         = x              id
  | B  -- x y z     = x (y z)        (.)
  | C  -- x y z     = (x z) y        flip
  | S' -- x y z w   = x (y w) (z w)
  | B' -- x y z w   = x y (z w)
  | C' -- x y z w   = x (y w) z
  | A  -- x y       = x              True
  | U  -- x y       = y x            uncurrg
  | Z  -- x y z     = x y
  | P  -- x y z     = (z x) y        (,)
  | R  -- x y z     = (y z) x
  | O  -- x y z w   = (w x) y        (:)
  | Y  -- x         = x (Y x)        fix
  | K2 -- x y z     = x
  | K3 -- x y z w   = x
  | K4 -- x y z w v = x
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
  deriving (Eq, Show)


table :: (Eq s, IsString s) => [(s, Combinator)]
table =
  [ ("S", S)
  , ("K", K)
  , ("I", I)
  , ("B", B)
  , ("C", C)
  , ("S'", S')
  , ("B'", B')
  , ("C'", C')
  , ("A", A)
  , ("U", U)
  , ("Z", Z)
  , ("P", P)
  , ("R", R)
  , ("O", O)
  , ("Y", Y)
  , ("K2", K2)
  , ("K3", K3)
  , ("K4", K4)
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
  ]


fromString :: (Eq s, IsString s) => s -> Maybe Combinator
fromString s = lookup (s) $ table


data Comb
  = CApp Comb Comb
  | CComb Combinator
  | CTemp Text -- need it temporarily
  | CIntLit Int
  deriving (Eq, Show)
