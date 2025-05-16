module Desugar.LC where

import Data.Text (Text)
import Prelude hiding (GT, LT)
import qualified OP


data Core
  = Var Text
  | Abs Text Core
  | App Core Core
  -- builtins
  | IntLit Int
  | Op OP.OP -- All operators are curried
  deriving (Eq, Show)
