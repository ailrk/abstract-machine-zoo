module Compiler.IMP where

import Machine
import qualified IMP
import Prelude hiding (LT, GT)
import Control.Monad.State


compile :: IMP.Stmt -> Program
compile stmt = evalState (compileStmt stmt) 0


compileStmt :: IMP.Stmt -> State Int Program
compileStmt stmt =
  case stmt of
    IMP.Assign n e -> do
      eprog <- compileExpr e
      store <- emit (STORE n)
      combine
        [ eprog
        , store
        ]
    IMP.Seq stmts -> mconcat <$> traverse compileStmt stmts
    IMP.If c t e -> do
      cprog <- compileExpr c
      compensate 1 -- compensate cond
      tprog@(Program(tinstrs, _)) <- compileStmt t
      compensate 1 -- compensate pass
      eprog@(Program(einstrs, _)) <- compileStmt e
      let cond = fromInstr (JZ (length tinstrs + 2))
      let pass = fromInstr (JUMP (length einstrs + 1))
      combine
        [ cprog
        , cond
        , tprog
        , pass
        , eprog
        ]
    IMP.While c s -> do
      cprog@(Program (cinstrs, _)) <- compileExpr c
      compensate 1 -- compensate cond
      sprog@(Program(sinstrs, _)) <- compileStmt s
      compensate 1 -- compensate back
      let cond = fromInstr (JZ (length sinstrs + 2))
      let back = fromInstr (JUMP (negate (length sinstrs + length cinstrs + 1)))
      combine
        [ cprog
        , cond
        , sprog
        , back
        ]
    IMP.Call f args -> do
      argsProg <- mconcat <$> traverse compileExpr args
      call <- emit (CALL f)
      combine [ argsProg, call ]
    IMP.FuncDef n args body -> do
      line <- get
      compensate 1 -- need to compensate for `skip`
      save <- mconcat <$> traverse (emit . STORE) (reverse args)
      bodyProg <- compileStmt body
      ret <- emit RETURN
      line' <- get
      let skip = fromInstr (JUMP (line' - line))
      combine $
        [ skip -- if fall through simply skip the function body
        , save
        , bodyProg
        , ret
        , addLabel (n, line + 1)
        ]
    IMP.Print e -> do
      exprProg <- compileExpr e
      p <- emit PRINT
      combine
        [ exprProg
        , p
        ]


compileExpr :: IMP.Expr -> State Int Program
compileExpr expr =
  case expr of
    IMP.Var x -> emit (LOAD x)
    IMP.IntLit n -> emit (PUSH n)
    IMP.BinOP op a b -> do
      aProg <- compileExpr a
      bProg <- compileExpr b
      opProg <- compileOp op
      combine [aProg, bProg, opProg]

    IMP.UnOP op a -> do
      aProg <- compileExpr a
      opProg <- compileOp op
      combine [aProg, opProg]


compileOp :: IMP.OP -> State Int Program
compileOp op =
  case op of
    IMP.Add -> emit ADD
    IMP.Sub -> emit SUB
    IMP.Mul -> emit MUL
    IMP.Div -> emit DIV
    IMP.Gt -> emit GT
    IMP.Gte -> emit GTE
    IMP.Lt -> emit LT
    IMP.Lte -> emit LTE
    IMP.Not -> emit NOT
    IMP.Eqv -> emit EQV
    IMP.Neq -> emit NEQ


addLabel :: Label -> Program
addLabel label = (Program ([], pure label))


combine :: [Program] -> State Int Program
combine = pure . mconcat


-- | Emit an instruction and line number
emit :: Instr -> State Int Program
emit instr = modify (+ 1) >> pure (Program ([instr], mempty))


fromInstr :: Instr -> Program
fromInstr instr = Program ([instr], mempty)


compensate :: Int -> State Int ()
compensate n = modify (+n)
