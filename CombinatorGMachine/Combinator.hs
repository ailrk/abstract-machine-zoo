module Combinator where
import Data.Text (Text)

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
  deriving (Eq, Show)


data Comb
  = CApp Comb Comb
  | CComb Combinator
  | CIntLit Int
  deriving (Eq, Show)
