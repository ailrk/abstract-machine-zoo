module Main where

import qualified LC
import qualified Compiler.LC
import qualified Machine
import Control.Monad.ST (runST)


eval :: LC.Expr -> IO ()
eval expr = do
  let de = Compiler.LC.desugar expr
  putStrLn (show expr)
  putStrLn (show de)

  let comb = Compiler.LC.compile expr
  let out = runST do
        ref <- Machine.toGraph comb
        Machine.normal ref >>= Machine.dump
  putStrLn out


main :: IO ()
main = do
  LC.run "data/lc/arith.lc" eval
  LC.run "data/lc/let.lc" eval
  LC.run "data/lc/if.lc" eval
  LC.run "data/lc/func.lc" eval
  LC.run "data/lc/rec.lc" eval
  LC.run "data/lc/closure.lc" eval
  LC.run "data/lc/nested.lc" eval
