{-# LANGUAGE LambdaCase #-}
module EvalApply.LC where

import qualified LC
import Data.Text (Text)
import LC (Expr(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader
import LC (Stmt)
import LC (Stmt(..))
import qualified OP
import System.IO (hFlush, stdout)
import Data.Char (ord)
import Control.Monad (foldM)
import Data.List (intercalate)

-- Eval brings a syntax to the execution context
-- Apply applies a function in the execution context.
-- It further require values to be evaluated.
-- Hence more Eval, hence more applies.
-- Continue until it ends.

type Env = Map Text Value

data Value
  = VInt Int
  | VBool Bool
  | VClosure [Text] Expr Env
  | VList [Value]
  | VPrim Text ([Value] -> EvalM Value)  -- for primitive ops


display :: Value -> String
display value =
  case value of
    VInt n -> show n
    VBool b -> show b
    VClosure {} -> "Closure"
    VList xs -> intercalate "," $ fmap display xs
    VPrim name _  -> "Prim " ++ show name


type EvalM a = ReaderT Env IO a


lookupEnv :: Text -> EvalM Value
lookupEnv sym = do
  env <- ask
  case Map.lookup sym env of
    Just v -> pure v
    Nothing -> error $ "lookup error, " ++ show sym


eval :: Expr -> EvalM Value
eval expr =
  case expr of
    Var n -> lookupEnv n
    Lam params body -> do
      env <- ask
      pure $ VClosure params body env
    App fexpr argsExpr -> do
      f <- eval fexpr
      args <- traverse eval argsExpr
      apply f args
    -- just sugars from here.
    Let x [] rhs body -> do
      env <- ask
      let f = VClosure [x] body env
      a <- eval rhs
      apply f [a]
    Let x params f body -> do
      env <- ask
      let closure = VClosure params f env
      local (Map.insert x closure) (eval body)
    LetRec x [] rhs body -> do
      env <- ask
      let f = VClosure [x] body env
      a <- eval rhs
      apply f [a]
    LetRec x params f body -> do
      env <- ask
      let closure = VClosure params f extendedEnv
          extendedEnv = Map.insert x closure env
      local (const extendedEnv) (eval body)
    If cond l r -> do
      VBool b <- eval cond
      case b of
        True -> eval l
        False -> eval r
    Lit x ->
      case x of
        LC.IntLit n -> pure $ VInt n
        LC.BoolLit b -> pure $ VBool b
    BinOP op l r -> do
      l' <- eval l
      r' <- eval r
      evalBinop op l' r'
    UnOP op x -> evalUnop op =<< eval x
    Do stmts
      | length stmts < 1 -> error "empty do block"
      | otherwise -> do
          foldM
            (\b stmt -> evalStmt stmt (pure b))
            (VInt 0)
            stmts
    List xs -> VList <$> traverse eval xs
  where
    evalBinop :: OP.OP -> Value -> Value -> EvalM Value
    evalBinop OP.Add (VInt l) (VInt r) = pure $ VInt (l + r)
    evalBinop OP.Sub (VInt l) (VInt r) = pure $ VInt (l - r)
    evalBinop OP.Mul (VInt l) (VInt r) = pure $ VInt (l * r)
    evalBinop OP.Div (VInt l) (VInt r) = pure $ VInt (l `div` r)
    evalBinop OP.Gt (VInt l) (VInt r) = pure $ VBool (l > r)
    evalBinop OP.Gte (VInt l) (VInt r) = pure $ VBool (l >= r)
    evalBinop OP.Lt (VInt l) (VInt r) = pure $ VBool (l < r)
    evalBinop OP.Lte (VInt l) (VInt r) = pure $ VBool (l <= r)
    evalBinop OP.Eqv (VInt l) (VInt r) = pure $ VBool (l == r)
    evalBinop _ _ _ = error "invalid binop"

    evalUnop OP.Not (VBool x) = pure $ VBool (not x)
    evalUnop _ _ = error "invalid unop"

    evalStmt :: Stmt -> EvalM Value -> EvalM Value
    evalStmt (LetBind x e) k = do
      v <- eval e
      local (Map.insert x v) k

    evalStmt (Action e) k = do
      _ <- eval e
      k


apply :: Value -> [Value] -> EvalM Value
apply (VClosure params body env) args
  | length params < length args = error $ "invalid arity " ++ show (length args) ++ ", expected " ++ show (length params)
  | otherwise = do
      let newEnv = Map.fromList (params `zip` args) `Map.union` env
      let unapplied = drop (length args) params -- handle partial application
      case unapplied of
        [] -> local (const newEnv) (eval body)
        _ -> pure $ VClosure unapplied body newEnv
apply (VPrim _ f) args  = f args
apply _ _ = error "can't apply non function"


initEnv :: Map Text Value
initEnv = Map.fromList
  [ ( "PRINT"
    , VPrim "PRINT" $ \case
          (s:_) -> do
            case s of
              VInt n -> do
                liftIO $ putStrLn (show n)
                pure (VInt 0)
              VBool n -> do
                liftIO $ putStrLn (show n)
                pure (VInt 0)
              _ -> error "can't print"
          _ -> error "arity"
    )
  , ( "READ"
    , VPrim "READ" $ \_-> do
        liftIO $ hFlush stdout
        line <- liftIO $ getLine
        case line of
          (c:_) -> pure $ VInt $ ord c
          [] -> pure $ VInt $ ord '\n'
      )
  , ( "CONS"
    , VPrim "CONS" $ \case
          x:s:_ -> do
            case s of
              VList [] -> pure (VList [x])
              VList xs -> pure (VList (x:xs))
              _ -> error $ "expect a list, got " ++ display s
          _ -> error "arity"
    )
  , ( "NIL"
    , VList []
    )
  , ( "NULL"
    , VPrim "NULL" $ \case
          s:_ -> do
            case s of
              VList [] -> pure (VBool True)
              VList _ -> pure (VBool False)
              _ -> error $ "expect a list, got " ++ display s
          _ -> error "arity"
    )
  , ( "HEAD"
    , VPrim "HEAD" $ \case
          s:_ -> do
            case s of
              VList [] -> error "head"
              VList (x:_) -> pure x
              _ -> error $ "expect a list, got " ++ display s
          _ -> error "arity"
    )
  , ( "TAIL"
    , VPrim "TAIL" $ \case
          s:_ -> do
            case s of
              VList [] -> pure $ VList []
              VList (_:xs) -> pure $ VList xs
              _ -> error $ "expect a list, got " ++ display s
          _ -> error "arity"
    )
  , ( "PURE"
    , VPrim "PURE" $ \case
          n:_ -> pure n
          _ -> error "arity"
    )
  ]
