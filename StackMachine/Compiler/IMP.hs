module Compiler.IMP where

import Machine
import qualified IMP
import Prelude hiding (LT, GT)
import Control.Monad.State
import Debug.Trace


compile :: IMP.Stmt -> Program
compile stmt = evalState (compileStmt stmt) 0


compileStmt :: IMP.Stmt -> State Int Program
compileStmt stmt =
  case stmt of
    IMP.Assign n e -> do
      eprog <- compileExpr e
      store <- emit [STORE n]
      combine
        [ eprog
        , store
        ]
    IMP.Seq stmts -> mconcat <$> traverse compileStmt stmts
    IMP.If c t e -> do
      cprog <- compileExpr c
      tprog@(Program(tinstrs, _)) <- compileStmt t
      eprog@(Program(einstrs, _)) <- compileStmt e
      cond <- emit [ JZ (length tinstrs + 2) ]
      pass <- emit [ JUMP (length einstrs + 1) ]
      combine
        [ cprog
        , cond
        , tprog
        , pass
        , eprog
        ]
    IMP.While c s -> do
      cprog@(Program (cinstrs, _)) <- compileExpr c
      sprog@(Program(sinstrs, _)) <- compileStmt s
      cond <- emit [JZ (length sinstrs + 2)]
      back <- emit [JUMP (negate (length sinstrs + length cinstrs + 1))]
      combine
        [ cprog
        , cond
        , sprog
        , back
        ]
    IMP.Call f args -> do
      argsProg <- mconcat <$> traverse compileExpr args
      call <- emit [CALL f]
      combine [ argsProg, call ]
    IMP.FuncDef n args body -> do
      line <- get <* compensate 1
      save <- emit $ fmap STORE (reverse args)
      bodyProg <- compileStmt body
      ret <- emit [RETURN]
      line' <- get
      let skip = Program ([JUMP (line' - line)], mempty)
      combine $
        [ skip -- skip function body, only way to execute function is to call
        , save
        , bodyProg
        , ret
        , addLabel (n, line + 1)
        ]
    IMP.Print e -> do
      exprProg <- compileExpr e
      p <- emit [PRINT]
      combine
        [ exprProg
        , p
        ]


compileExpr :: IMP.Expr -> State Int Program
compileExpr expr =
  case expr of
    IMP.Var x -> emit [LOAD x]
    IMP.IntLit n -> emit [PUSH n]
    IMP.BinOP op a b -> do
      aProg <- compileExpr a
      bProg <- compileExpr b
      opProg <- compileOp op
      combine [aProg, bProg, opProg]

    IMP.UNOP op a -> do
      aProg <- compileExpr a
      opProg <- compileOp op
      combine [aProg, opProg]


compileOp :: IMP.OP -> State Int Program
compileOp op =
  case op of
    IMP.Add -> emit [ADD]
    IMP.Sub -> emit [SUB]
    IMP.Mul -> emit [MUL]
    IMP.Div -> emit [DIV]
    IMP.GT -> emit [GT]
    IMP.GTE -> emit [GTE]
    IMP.LT -> emit [LT]
    IMP.LTE -> emit [LTE]
    IMP.NOT -> emit [NOT]


addLabel :: Label -> Program
addLabel label = (Program ([], pure label))



combine :: [Program] -> State Int Program
combine = pure . mconcat


emit :: [Instr] -> State Int Program
emit instrs = modify (+ (length instrs)) >> pure (Program (instrs, mempty))


compensate :: Int -> State Int ()
compensate n = modify (+n)
