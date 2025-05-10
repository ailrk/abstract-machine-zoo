module Main where

import qualified LC


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
  putStrLn "hi"
