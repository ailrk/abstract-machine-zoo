module Main where

import qualified LC
import Combinator (Comb(..), Combinator(..))
import qualified Machine
import Control.Monad.ST (runST)


eval :: Show a => a -> IO ()
eval expr = do
  putStrLn (show expr)


main :: IO ()
main = do
  LC.run "data/lc/let.lc" eval
  LC.run "data/lc/if.lc" eval
  LC.run "data/lc/func.lc" eval
  LC.run "data/lc/rec.lc" eval
  LC.run "data/lc/closure.lc" eval
  LC.run "data/lc/nested.lc" eval

  let c1 = CApp
            (CApp
              (CApp
                (CComb S)
                (CComb MUL))
              (CComb I))
            (CApp
              (CApp
                (CComb ADD)
                (CIntLit 2))
              (CIntLit 3))

  let r = runST $ do
            ref <- Machine.toGraph c1
            Machine.normal ref >>= Machine.dump
  putStrLn r
