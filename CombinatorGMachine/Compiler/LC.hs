module Compiler.LC where

import Data.Text (Text)
import qualified LC
import Combinator
import Data.List ((\\))
import Prelude hiding (GT, LT)
import Data.Function ((&))


freeVars :: Core -> [Text]
freeVars c =
  case c of
    Var x -> [x]
    Abs x expr ->
      let fvs = freeVars expr
       in fvs \\ [x]
    App x y ->
      freeVars x ++ freeVars y
    Op _ -> []
    IntLit _ -> []


scottBool :: Bool -> Core
scottBool True = Var "K"
scottBool False = Var "K" `App` Var "I"


-- Force the toplevel lazy IO by wrapping it as
-- a dependency of another IO monad.
forceToplevelIO :: Core -> Core
forceToplevelIO core =
  App
    (App (Var "BIND") core)
    (Abs "_" (App (Var "PURE") (IntLit 0)))


desugar :: LC.Expr -> Core
desugar expr =
  case expr of
    LC.Var val -> Var val
    LC.Lam params body -> foldr (\a b -> Abs a b) (desugar body) params
    LC.App f args -> foldr (\a b -> b `App` (desugar a)) (desugar f) (reverse args)
    LC.Let name params val body
      | [] <- params -> Abs name (desugar body) `App` desugar val
      | otherwise -> desugar $ LC.Let name [] (LC.Lam params val) body
    -- = Y (\self.(\x....))
    LC.LetRec name params val body
      | [] <- params ->
          App
            (Abs name (desugar body))
            (App (Var "Y") (Abs name (desugar val)))
      | otherwise -> desugar $ LC.LetRec name [] (LC.Lam params val) body
    LC.If cond t e -> do
      let cond' = desugar cond -- scott
      let t' = desugar t
      let e' = desugar e
      cond' `App` t' `App` e'
    LC.Lit lit ->
      case lit of
        LC.IntLit n -> IntLit n
        LC.BoolLit b -> scottBool b
    LC.BinOP op l r -> (Op op) `App` (desugar l) `App` (desugar r)
    LC.UnOP op a -> (Op op) `App` (desugar a)
    LC.Do stmts -> do
      let seq' = foldr (.) id $ fmap desugarStmt (init stmts)
      seq' $ case last stmts of
             LC.Action action -> do
               desugar action
             _ -> error "last statement in do must be an exprssion"
    LC.List elems -> do -- scott
      elems
        & fmap desugar
        & foldr (\e es -> Var "CONS" `App` e `App` es) (Var "NIL")


-- To super combinators
rewrite :: Core -> Core
rewrite core =
  case core of
    Var "I" `App` n@(IntLit _) -> n
    Var "I" `App` n@(Op _) -> n
    Var "S" `App` (Var "K" `App` x) `App` (Var "K" `App` y) -> Var "K" `App` (x `App` y)
    Var "S" `App` (Var "K" `App` x) `App` y -> Var "B" `App` x `App` y
    (Var "S" `App` x) `App` (Var "K" `App` y) -> Var "C" `App` x `App` y
    Var "B" `App` x `App` Var "I" -> x
    Var "B" `App` Var "I" `App` x -> x
    x `App` y -> rewrite x `App` rewrite y
    _ -> core


opt :: Core -> Core
opt core =
  if optimized == core
     then optimized
     else case optimized of
            (x `App` y) -> opt (opt x `App` opt y)
            _ -> opt core
  where
    optimized = rewrite core


desugarStmt :: LC.Stmt -> (Core -> Core)
desugarStmt stmt =
  case stmt of
    LC.LetBind x action -> \k -> (Var "BIND" `App` desugar action) `App` (Abs x k)
    LC.Action action -> \k -> (Var "BIND" `App` desugar action) `App` (Abs "_" k)


operator :: LC.OP -> Combinator
operator o =
  case o of
    LC.Add -> ADD
    LC.Sub -> SUB
    LC.Mul -> MUL
    LC.Div -> DIV
    LC.Gt -> GT
    LC.Gte -> GTE
    LC.Lt -> LT
    LC.Lte -> LTE
    LC.Eqv -> EQV
    LC.Neq -> NEQ
    LC.Not -> NOT


-- Notation: [x]N is a lambda term extentionally equal to \x.M
-- Bracket abstraction:
-- [x]x   = I
-- [x]M   = K M  -- if x not free in M
-- [x]M N = S [x]M [x]N
bracket :: Core -> Core
bracket core =
  case core of
    Var _ -> elim core
    Abs {} -> elim core
    App a b -> elim a `App` elim b
    IntLit _ -> core
    Op _ -> core
  where
    elim c =
      case c of
        Var x -> Var x
        Abs x (Var y) | x == y -> Var "I"
        Abs _ (IntLit n) -> Var "K" `App` IntLit n
        Abs _ (Op op) -> Var "K" `App` Op op
        Abs x expr | not (x `elem` freeVars expr) ->
          Var "K" `App` bracket expr
        Abs x (Abs y body) ->
          bracket (Abs x (bracket (Abs y body)))
        Abs x (App y z) ->
          Var "S"
            `App` bracket (Abs x y)
            `App` bracket (Abs x z)
        _ -> c


compile0 :: Core -> Core
compile0 core =
  case bracket core of
    Var n ->
      case lookup n Combinator.table of
        Just _ -> Var n
        _ ->
          case lookup n Combinator.builtins of
            Just expr -> expr
            _ -> error $ "unexpected free variable " ++ show n
    Abs {} -> error "unexpected lambda"
    App x y -> (compile0 x) `App` (compile0 y)
    x@(IntLit _) -> x
    x@(Op _) -> x


compile1 :: Core -> Comb
compile1 core =
  case bracket core of
    Var n ->
      case lookup n Combinator.table of
        Just comb -> CComb comb
        _ -> error $ "unexpected free variable " ++ show n
    Abs {} -> error "unexpected lambda"
    App x y -> CApp (compile1 x) (compile1 y)
    IntLit n -> CIntLit n
    Op op -> CComb (operator op)


compile :: LC.Expr -> Comb
compile
  = compile1
  . opt
  . compile0
  . forceToplevelIO
  . desugar
