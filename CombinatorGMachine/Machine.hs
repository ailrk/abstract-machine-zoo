{-# OPTIONS_GHC -Wno-orphans #-}
module Machine where

import Combinator (Combinator(..), Comb(..))
import Prelude hiding (LT, GT)
import Data.Char (ord)
import Data.IORef (IORef)
import GHC.IORef (writeIORef, readIORef, newIORef)
import System.IO (hFlush, stdout)

-- Graph reduction on SKI combinators

newtype GRef = GRef (IORef Graph)
  deriving Eq

gnew :: Graph -> IO GRef
gnew g = GRef <$> newIORef g

gread :: GRef -> IO Graph
gread (GRef ref) = readIORef ref

gwrite :: GRef -> Graph -> IO ()
gwrite (GRef ref) g  = writeIORef ref g


infix 5 :@

data Graph
  = GRef :@ GRef
  | Comb Combinator
  | IntLit Int
  | Action (IO Graph)


dump :: GRef -> IO String
dump ref = do
  g <- gread ref
  case g of
    Comb c -> pure $ show c
    IntLit n -> pure $ show n
    Action _ -> pure "{IO}"
    r1 :@ r2 -> do
      s1 <- dump r1
      s2 <- dump r2
      pure $ "(" ++ s1 ++ " @ " ++ s2 ++ ")"


newtype Spine = Spine [GRef]


-- encode into graph
toGraph :: Comb -> IO GRef
toGraph c =
  case c of
    CApp l r -> do
      lref <- toGraph l
      rref <- toGraph r
      gnew $ lref :@ rref
    CComb comb -> gnew $ Comb comb
    CIntLit n -> gnew $ IntLit n
    CTemp _ -> error "impossible"


-- collect left most spines
spine :: GRef -> IO Spine
spine ref = go ref (Spine [])
  where
    go gref (Spine stack) = do
      graph <- gread gref
      case graph of
        l :@ _ -> go l (Spine (gref:stack))
        _ -> pure (Spine (gref:stack))


-- reduce one step
step :: GRef -> IO ()
step ref = do
  Spine (n:rest) <- spine ref
  redex <- gread n
  case redex of
    IntLit _ | not . null $ rest -> error "Literal can't be at the lhs"
    Comb k -> reduce k (Spine rest)
    _ -> pure ()



-- reduce to nf form
nf :: GRef -> IO GRef
nf ref = do
  step ref
  graph <- gread ref
  case graph of
    _ :@ _ -> nf ref
    _ -> pure ref


whnf :: GRef -> IO GRef
whnf ref = do
  step ref
  graph <- gread ref
  case graph of
    x :@ y -> do
      x' <- whnf x
      gwrite ref (x' :@ y)
      pure ref
    _ -> pure ref


reduce :: Combinator -> Spine -> IO ()
reduce comb (Spine stack) = do
  case (comb, stack) of
    (S, r1:r2:r3:_) -> do
      (_ :@ x) <- gread r1
      (_ :@ y) <- gread r2
      (_ :@ z) <- gread r3
      liftA2 (:@) (gnew $ x :@ z) (gnew $ y :@ z)
        >>= gwrite r3
    (I, r:_) -> do
      (_ :@ x) <- gread r
      gread x >>= gwrite r
    (K, r1:r2:_) -> do
      (_ :@ x) <- gread r1
      (_ :@ _) <- gread r2
      gread x >>= gwrite r2
    (B, r1:r2:r3:_) -> do
      (_ :@ x) <- gread r1
      (_ :@ y) <- gread r2
      (_ :@ z) <- gread r3
      liftA2 (:@)
        (pure x)
        (gnew $ y :@ z)
        >>= gwrite r3
    (C, r1:r2:r3:_) -> do
      (_ :@ x) <- gread r1
      (_ :@ y) <- gread r2
      (_ :@ z) <- gread r3
      liftA2 (:@)
        (gnew $ x :@ z)
        (pure y)
        >>= gwrite r3
    (A, r1:r2:_) -> do
      (_ :@ _) <- gread r1
      (_ :@ y) <- gread r2
      gread y >>= gwrite r2
    (U, r1:r2:_) -> do
      (_ :@ x) <- gread r1
      (_ :@ y) <- gread r2
      gwrite r2 (y :@ x)
    (Z, r1:r2:r3:_) -> do
      (_ :@ x) <- gread r1
      (_ :@ y) <- gread r2
      gwrite r3 (x :@ y)
    (P, r1:r2:r3:_) -> do
      (_ :@ x) <- gread r1
      (_ :@ y) <- gread r2
      (_ :@ z) <- gread r3
      liftA2 (:@)
        (gnew $ z :@ x)
        (pure y)
        >>= gwrite r3
    (R, r1:r2:r3:_) -> do
      (_ :@ x) <- gread r1
      (_ :@ y) <- gread r2
      (_ :@ z) <- gread r3
      liftA2 (:@)
        (gnew $ y :@ z)
        (pure x)
        >>= gwrite r3
    (O, r1:r2:r3:r4:_) -> do
      (_ :@ x) <- gread r1
      (_ :@ y) <- gread r2
      (_ :@ _) <- gread r3
      (_ :@ w) <- gread r4
      liftA2 (:@)
        (gnew $ w :@ x)
        (pure y)
        >>= gwrite r4
    (Y, r1:_) -> do
      (_ :@ x) <- gread r1
      gwrite r1 $ x :@ r1
    (ADD, r1:r2:_) -> binop (+) r1 r2
    (SUB, r1:r2:_) -> binop (-) r1 r2
    (MUL, r1:r2:_) -> binop (*) r1 r2
    (DIV, r1:r2:_) -> binop div r1 r2
    (GT, r1:r2:_) -> cmp (>) r1 r2
    (GTE, r1:r2:_) -> cmp (>=) r1 r2
    (LT, r1:r2:_) -> cmp (<) r1 r2
    (LTE, r1:r2:_) -> cmp (<=) r1 r2
    (EQV, r1:r2:_) -> cmp (==) r1 r2
    (NEQ, r1:r2:_) -> cmp (/=) r1 r2
    (NOT, r1:_) -> not' r1
    (NIL, r1:_) -> do
      (self :@ _) <- gread r1
      gwrite self (Comb K)
    (CONS, r1:_:_:_:_) -> do
      (self :@ _) <- gread r1
      gwrite self (Comb O)
    (NULL, r1:_) -> undefined
    (HEAD, r1:_) -> undefined
    (TAIL, r1:_) -> undefined
    (PURE, r1:_) -> do
      (_ :@ x) <- gread r1
      x' <- nf x
      g <- gread x'
      gwrite r1 $ Action do
        pure g
    (BIND, r1:r2:_) -> do
      (_ :@ x) <- gread r1
      (_ :@ y) <- gread r2
      x' <- nf x -- evaluate IO code to Action. need normal form.
      Action m <- gread x'
      a <- m -- force the action
      f <- gread y
      liftA2 (:@)
        (gnew f)
        (gnew a)
        >>= gwrite r2
    (PRINT, r1:_) -> do
      (_ :@ x) <- gread r1
      str <- nf x >>= dump
      gwrite r1 $
        Action do
          putStrLn str
          pure (IntLit 0)
    (READ, r1:_) -> do
      gwrite r1 $
        Action do
          hFlush stdout
          line <- getLine
          case line of
            (c:_) -> pure $ IntLit $ ord c
            [] -> pure $ IntLit $ ord '\n'
    (ERROR, _) -> error "panic"
    _ -> error ("invalid graph, " <> show comb)


cmp :: (Int -> Int -> Bool) -> GRef -> GRef -> IO ()
cmp op r1 r2 = do
  (_ :@ x) <- gread r1
  (_ :@ y) <- gread r2
  IntLit x' <- nf x >>= gread
  IntLit y' <- nf y >>= gread
  bool <- case op x' y' of
            True -> toGraph $ CComb K
            False -> toGraph $ CComb K `CApp` CComb I
  gread bool >>= gwrite r2


not' :: GRef -> IO ()
not' r1 = do
  (_ :@ x) <- gread r1
  b <- gread x
  case b of
    Comb K -> do
      true <- liftA2 (:@) (gnew (Comb K)) (gnew (Comb I))
      gwrite r1 true
    l :@ r -> do
      k <- gread l
      i <- gread r
      case (k, i) of
        (Comb K, Comb I) -> do
          gwrite r1 (Comb K)
        _ -> invalid
    _ -> invalid
  where
    invalid = error "Invalid scott boolean"


binop :: (Int -> Int -> Int) -> GRef -> GRef -> IO ()
binop op r1 r2 = do
  (_ :@ x) <- gread r1
  (_ :@ y) <- gread r2
  IntLit x' <- nf x >>= gread
  IntLit y' <- nf y >>= gread
  gwrite r2 (IntLit $ op x' y')
