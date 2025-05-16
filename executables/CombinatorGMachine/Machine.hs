module Machine where

import Combinator (Combinator(..), table)
import Data.Vector.Mutable (RealWorld, MVector)
import qualified Data.Vector.Mutable as MVector
import Control.Monad.State
import Control.Monad (forM_)
import qualified Data.List as List
import Prelude hiding (GT, LT)

type Addr = Int

data Val
  = IntLit {-# UNPACK #-} !Int
  | Comb {-# UNPACK #-} !Combinator
  | Arg {-# UNPACK #-} !Addr


data Instr
  = Push {-# UNPACK #-} !Val
  | Slide {-# UNPACK #-} !Int
  | Mkap
  | Eval
  | Unwind


infix 5 :@


data Node
  = {-# UNPACK #-} !Addr :@ {-# UNPACK #-} !Addr
  | NIntLit {-# UNPACK #-} !Int
  | NComb {-# UNPACK #-} !Combinator
  | NRef {-# UNPACK  #-} !Int -- indirection node
  | NDummy


data Heap = Heap Int (MVector RealWorld Node)
type Code = [Instr]
type Stack = [Addr]
type Dump = [(Stack, Code)]


data Machine = Machine
  { _heap :: Heap
  , _code :: Code
  , _stack :: Stack
  , _dump :: Dump
  }


type GMachineM a = StateT Machine IO a


newMachine :: Code -> IO Machine
newMachine code = do
  buffer <- MVector.replicate 2048 NDummy
  forM_ ([0..] `zip` table) $ \(idx, (_, c)) -> do
    allocate' (NComb c) (Heap idx buffer)
  pure Machine
    { _heap = Heap (length table) buffer
    , _code = code
    , _stack = []
    , _dump = []
    }


allocate' :: Node -> Heap -> IO Heap
allocate' node (Heap i heap) = do
  let i' = i + 1
  let size = MVector.length heap
  newHeap <-
    if i' > size
       then do
         v <- MVector.grow heap (i' + i' `div` 2)
         MVector.set (MVector.slice i' (MVector.length v - i') v) NDummy
         pure v
       else pure heap
  MVector.write newHeap i' node
  pure $ Heap i' newHeap


allocate :: Node -> GMachineM Addr
allocate node = do
  heap <- _heap <$> get
  heap'@(Heap i _) <- liftIO $ allocate' node heap
  modify' (\s -> s { _heap = heap'})
  pure i


peek :: GMachineM Node
peek = do
  (x:_) <- _stack <$> get
  Heap _ heap <- _heap <$> get
  MVector.read heap x


final :: GMachineM Bool
final = do
  code <- _code <$> get
  dump <- _dump <$> get
  stack <- _stack <$> get
  Heap _ heap <- _heap <$> get
  isWhnf <- case stack of
              [addr] -> do
                isLiteral <$> MVector.read heap addr
              _ -> pure False
  pure $ null code -- node more code
      || (null dump && isWhnf) -- whnf and no return


isLiteral :: Node -> Bool
isLiteral val =
  case val of
    NIntLit _ -> True
    NDummy -> error "impossible"
    _ -> False


eval :: Instr -> GMachineM ()
eval instr = do
  Heap _ heap <- _heap <$> get
  case instr of
    Push val -> do
      case val of
        Arg i -> do
          stack <- _stack <$> get
          _ :@ a <- MVector.read heap (stack !! (i + 1))
          modify' $ \s -> s { _stack = a:stack }
        Comb c -> do
          case List.findIndex (\(_, k) -> c == k) table of
            Just i -> push i
            Nothing -> error "impossible"
        IntLit n -> do
          addr <- allocate (NIntLit n)
          push addr
    Mkap -> do
      x:f:_ <- _stack <$> get
      addr <- allocate (f :@ x)
      push addr
    Slide n -> modify' $ \s -> s { _stack = drop n $ _stack s }
    Eval -> do
      x:xs <- _stack <$> get
      code <- _code <$> get
      modify' $ \s ->
        s { _dump = [(xs, code)]
          , _code = [Unwind]
          , _stack = [x]
          }
    Unwind -> do
      stack@(x:_) <- _stack <$> get
      node <- MVector.read heap x
      case node of
        NIntLit _ -> do
          dump <- _dump <$> get
          case dump of
            (stack', code'):dump' -> ret (x:stack') code' dump'
            [] -> modify' $ \s -> s { _code = [] }
        f :@ _ ->
          modify' $ \s ->
            s { _stack = f:stack
              , _code = [Unwind]
              }
        NRef _ -> undefined
        NComb comb -> undefined
        NDummy -> error "impossible"
      undefined
  where
    push = \i ->
      modify' $ \s -> s { _stack = i:_stack s }
    ret = \stack' code' dump' -> do
      modify' $ \s ->
        s { _dump = dump'
          , _code = code'
          , _stack = stack'
          }


apply :: Combinator -> [Instr]
apply comb =
  case comb of
    S -> []
    K -> []
    I -> []
    B -> []
    C -> []
    A -> []
    U -> []
    Z -> []
    P -> []
    R -> []
    O -> []
    Y -> []
    ADD -> []
    SUB -> []
    MUL -> []
    DIV -> []
    GT -> []
    GTE -> []
    LT -> []
    LTE -> []
    NOT -> []
    EQV -> []
    NEQ -> []
    PRINT -> []
    READ -> []
    BIND -> []
    PURE -> []
    ERROR -> []
