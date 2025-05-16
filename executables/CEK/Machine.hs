module Machine where
import Data.Text (Text)
import OP (OP(..))


type Symbol = Text
type Env = [(Symbol, Closure)]
data Closure = Closure Core Env deriving (Eq, Show)


data Core
  = Var Text
  | Abs Text Core
  | App Core Core
  -- builtins
  | IntLit Int
  | Op OP -- All operators are curried
  deriving (Eq, Show)


-- Feature if CEK is explicit continuation
data Kont
  = Empty            -- empty continuation
  | Ar Core Env Kont -- argument frame
  | Fn Closure Kont  -- function frame
  deriving (Eq, Show)


data CEK = CEK
  { _expr :: Core -- (C)ontrol
  , _env :: Env   -- (E)nvironment
  , _cont :: Kont -- (K)ontinuation
  }
  deriving (Eq, Show)
