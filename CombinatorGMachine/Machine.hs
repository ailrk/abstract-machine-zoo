{-# OPTIONS_GHC -Wno-orphans #-}
module Machine where

import Control.Monad.ST (ST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Combinator (Combinator(..), Comb(..))
import Prelude hiding (LT, GT)

-- Graph reduction on SKI combinators

newtype GRef s = GRef (STRef s (Graph s))
  deriving Eq

gnew :: Graph s -> ST s (GRef s)
gnew g = GRef <$> newSTRef g

gread :: GRef s -> ST s (Graph s)
gread (GRef ref) = readSTRef ref

gwrite :: GRef s -> Graph s -> ST s ()
gwrite (GRef ref) g  = writeSTRef ref g


instance MonadFail (ST s) where
  fail = error


infix 5 :@

data Graph s
  = GRef s :@ GRef s
  | Comb Combinator
  | IntLit Int
  deriving Eq


dump :: GRef s -> ST s String
dump ref = do
  g <- gread ref
  case g of
    Comb c -> pure $ show c
    IntLit n -> pure $ show n
    r1 :@ r2 -> do
      s1 <- dump r1
      s2 <- dump r2
      pure $ "(" ++ s1 ++ " @ " ++ s2 ++ ")"


newtype Spine s = Spine [GRef s]


-- encode into graph
toGraph :: Comb -> ST s (GRef s)
toGraph c =
  case c of
    CApp l r -> do
      lref <- toGraph l
      rref <- toGraph r
      gnew $ lref :@ rref
    CComb comb -> gnew $ Comb comb
    CIntLit n -> gnew $ IntLit n


-- collect left most spines
spine :: GRef s -> ST s (Spine s)
spine ref = go ref (Spine [])
  where
    go gref (Spine stack) = do
      graph <- gread gref
      case graph of
        l :@ _ -> go l (Spine (gref:stack))
        _ -> pure (Spine (gref:stack))


-- reduce one step
step :: GRef s -> ST s ()
step ref = do
  Spine (n:rest) <- spine ref
  redex <- gread n
  case redex of
    Comb k -> reduce k (Spine rest)
    _ -> pure ()


-- reduce to normal form
normal :: GRef s -> ST s (GRef s)
normal ref = do
  step ref
  graph <- gread ref
  case graph of
    _ :@ _ -> normal ref
    _ -> pure ref


reduce :: Combinator -> Spine s -> ST s ()
reduce k (Spine stack) = do
  case (k, stack) of
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
      (_ :@ _) <- gread r1
      (_ :@ y) <- gread r2
      gread y >>= gwrite r2
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
    (S', r1:r2:r3:r4:_) -> do
      (_ :@ x) <- gread r1
      (_ :@ y) <- gread r2
      (_ :@ z) <- gread r3
      (_ :@ w) <- gread r4
      liftA2 (:@)
        (pure x)
        (liftA2 (:@)
          (gnew $ y :@ w)
          (gnew $ z :@ w) >>= gnew)
        >>= gwrite r4
    (B', r1:r2:r3:r4:_) -> do
      (_ :@ x) <- gread r1
      (_ :@ y) <- gread r2
      (_ :@ z) <- gread r3
      (_ :@ w) <- gread r4
      liftA2 (:@)
        (pure x)
        (liftA2 (:@)
          (pure y)
          (gnew $ z :@ w) >>= gnew)
        >>= gwrite r4
    (C', r1:r2:r3:r4:_) -> do
      (_ :@ x) <- gread r1
      (_ :@ y) <- gread r2
      (_ :@ z) <- gread r3
      (_ :@ w) <- gread r4
      liftA2 (:@)
        (pure x)
        (liftA2 (:@)
          (gnew $ y :@ w)
          (pure z) >>= gnew)
        >>= gwrite r4
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
    (K2, r1:_:r3:_) -> do
      (_ :@ x) <- gread r1
      gread x >>= gwrite r3
    (K3, r1:_:_:r4:_) -> do
      (_ :@ x) <- gread r1
      gread x >>= gwrite r4
    (K3, r1:_:_:_:r5:_) -> do
      (_ :@ x) <- gread r1
      gread x >>= gwrite r5
    (ADD, r1:r2:_) -> binop (+) r1 r2
    (SUB, r1:r2:_) -> binop (-) r1 r2
    (MUL, r1:r2:_) -> binop (*) r1 r2
    (DIV, r1:r2:_) -> binop div r1 r2
    (GT, r1:r2:_) -> binop (\a b -> if a > b then 1 else 0) r1 r2
    (GTE, r1:r2:_) -> binop (\a b -> if a >= b then 1 else 0) r1 r2
    (LT, r1:r2:_) -> binop (\a b -> if a < b then 1 else 0) r1 r2
    (LTE, r1:r2:_) -> binop (\a b -> if a <= b then 1 else 0) r1 r2
    (EQV, r1:r2:_) -> binop (\a b -> if a == b then 1 else 0) r1 r2
    (NOT, r1:_) -> unop (\a -> if a > 0 then 0 else 1) r1
    _ -> do
      error ("invalid graph, " <> show k)


binop :: (Int -> Int -> Int) -> GRef s -> GRef s -> ST s ()
binop op r1 r2 = do
  (_ :@ x) <- gread r1
  (_ :@ y) <- gread r2
  IntLit x' <- normal x >>= gread
  IntLit y' <- normal y >>= gread
  gwrite r2 (IntLit $ op x' y')


unop :: (Int -> Int) -> GRef s -> ST s ()
unop op r1 = do
  (_ :@ x) <- gread r1
  IntLit x' <- normal x >>= gread
  gwrite r1 (IntLit $ op x')
