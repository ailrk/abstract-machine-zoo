{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Machine where

import Control.Monad.State
import Data.Text (Text, unpack)
import Prelude hiding (LT, GT)


data Machine = Machine
  { _stack :: Stack
  , _callStack :: CallStack
  , _env :: Env
  , _ip :: IP
  }
  deriving (Show)

type CallStack = [(Int, Env)]
type Addr = Int
type IP = Addr
type StackM = StateT Machine IO ()
type Env = [(Text, Int)]
type Stack = [Int]
type Id = Text
type Label = (Id, Addr)
newtype Program = Program ([Instr], [Label]) deriving (Show)

instance Semigroup Program where
  Program (is1, ls1) <> Program (is2, ls2) = Program (is1 <> is2, ls1 <> ls2)
instance Monoid Program where
  mempty = Program (mempty, mempty)


data Instr
  = PUSH Int
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
  | STORE Id
  | LOAD Id
  | JUMP Int -- relative jump
  | JZ Int   -- relative jump
  | CALL Id
  | RETURN
  | READ
  | PRINT
  | HALT
  deriving (Show)


replace :: Eq k => (k, v) -> [(k, v)] -> [(k, v)]
replace (k, v) xs = (k, v):filter ((/= k) . fst) xs

exec :: [Label] -> Instr -> StackM
exec labels instr =
  case instr of
    (PUSH n) -> do
      modify' (\s -> s { _stack = n : _stack s })
    ADD -> binOp (+)
    SUB -> binOp (-)
    MUL -> binOp (*)
    DIV -> binOp div
    STORE i -> do
      s <- get
      case _stack s of
        x:xs -> do
          put $ s { _stack = xs
                  , _env = replace (i,x) (_env s)
                  }
        _ -> panic "Stack underflow on STORE"
    LOAD i -> do
      s <- get
      let lookupEnv env =
            case lookup i env of
              Just val -> put $ s { _stack = val:_stack s }
              Nothing -> panic $ "Variable " ++ unpack i ++ " not found"
      lookupEnv (_env s)
    GT -> binOp (\x y -> if x > y then 1 else 0)
    GTE -> binOp (\x y -> if x >= y then 1 else 0)
    LT -> binOp (\x y -> if x < y then 1 else 0)
    LTE -> binOp (\x y -> if x <= y then 1 else 0)
    EQV -> binOp (\x y -> if x == y then 1 else 0)
    NOT -> unary (\x -> if x > 0 then 0 else 1)
    JUMP idx -> modify' (\s -> s { _ip = _ip s + idx})
    JZ idx -> do
      s <- get
      case _stack s of
        0:xs -> put (s { _stack = xs, _ip = _ip s + idx })
        _:xs -> put (s { _stack = xs, _ip = _ip s + 1 })
        _ -> panic $ "Stack underflow on JZ"
    CALL label -> do
      s <- get
      let retAddr = _ip s + 1
      let newCallStack = (retAddr, _env s) : _callStack s
      let mAddr = lookup label labels
      case mAddr of
        Just addr -> put (s { _ip = addr, _callStack = newCallStack, _env = [] })
        Nothing -> panic "Unknown function on CALL"
    RETURN -> do
      s <- get
      case _callStack s of
        (retAddr, savedEnv):res -> do
          put (s { _env = savedEnv, _callStack = res, _ip = retAddr })
        _ -> panic "Callstack underflow on RETURN"
    PRINT -> do
      s <- get
      case _stack s of
        x:xs -> do
          liftIO (print x)
          put (s { _stack = xs })
        _ -> panic "Stack underflow on PRINT"
    READ -> do
      c <- liftIO $ read . pure <$> getChar
      modify' (\s -> s { _stack = c : _stack s})
    HALT -> liftIO $ putStrLn "Halting"


panic :: String -> StackM
panic msg = do
  line <- get
  fail (msg ++ ", machine: " ++ show line)


unary :: (Int -> Int) -> StackM
unary f = do
  s <- get
  case _stack s of
    x:rest -> put (s { _stack = f x : rest })
    _ -> liftIO $ putStrLn "Stack overflow on unary operation"


binOp :: (Int -> Int -> Int) -> StackM
binOp f = do
  s <- get
  case _stack s of
    x:y:rest -> put (s { _stack = f y x : rest })
    _ -> liftIO $ putStrLn "Stack overflow on binary operation"


run :: Program -> StackM
run (Program (instrs, labels)) = do
  s <- get
  if _ip s >= length instrs
     then return ()
     else do
       let i = instrs !! _ip s
       exec labels i
       inc i
       run $ Program (instrs, labels)
  where
    inc :: Instr -> StackM
    inc (JUMP _) = pure ()
    inc (JZ _) = pure ()
    inc (CALL  _) = pure ()
    inc RETURN = pure ()
    inc _ = modify (\s -> s { _ip = _ip s + 1 })


start :: Program -> IO ()
start prog = do
  _ <- runStateT (run prog) (Machine [] [] [] 0)
  pure ()
