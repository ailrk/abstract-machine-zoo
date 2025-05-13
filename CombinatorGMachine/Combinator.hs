module Combinator where
import Data.Text (Text)
import Prelude hiding (GT, LT)
import qualified LC
import qualified Data.Text as Text

-- The combinator language

data Combinator
  = S  -- x y z     = x z (y x)      (<*>)
  | K  -- x y       = x              const, True, []
  | I  -- x         = x              id
  | B  -- x y z     = x (y z)        (.)
  | C  -- x y z     = (x z) y        flip
  | A  -- x y       = y              False
  | U  -- x y       = y x            uncurrg
  | Z  -- x y z     = x y
  | P  -- x y z     = (z x) y        (,)
  | R  -- x y z     = (y z) x
  | O  -- x y z w   = (w x) y        (:)
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


data Core
  = Var Text
  | Abs Text Core
  | App Core Core
  -- builtins
  | IntLit Int
  | Op LC.OP -- All operators are curried
  deriving (Eq, Show)


displayCore :: Core -> String
displayCore core =
  case core of
    Var n -> Text.unpack n
    Abs x body -> "\\" ++ Text.unpack x ++ "." ++ displayCore body ++ ""
    App a b -> "(" ++ displayCore a ++ " @ " ++ displayCore b ++ ")"
    IntLit n -> show n
    Op op -> show op


builtins :: [(Text, Core)]
builtins =
  [ -- NIL = λnil cons. nil
    ("NIL" , Var "K")
    -- CONS = λx xs. λnil cons. cons x xs
  , ("CONS"
    , Abs "x"
        (Abs "xs"
          (Abs "nil"
            (Abs "cons"
              (Var "cons" `App` Var "x" `App` Var "xs"))))
    )
    -- NULL = λl. l  true  false
    --  where true = K false = K I
  , ("NULL"
    , Abs "l"
        (Abs "t"
          (Abs "f"
            (Var "l"
              `App` Var "t"
              `App` (Abs "_1" (Abs "_2" (Var "f"))))))
    )
    -- HEAD = λl. l err K
  , ("HEAD"
    , Abs "l"
        (Var "l"
          `App` Var "ERROR"
          `App` Var "K")
    )
    -- TAIL = λl. l err A
  , ("TAIL"
    , Abs "l"
        (Var "l"
          `App` Var "ERROR"
          `App` Var "A")
    )
  ]


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


data Comb
  = CApp Comb Comb
  | CComb Combinator
  | CTemp Text -- need it temporarily
  | CIntLit Int
  deriving (Eq, Show)
