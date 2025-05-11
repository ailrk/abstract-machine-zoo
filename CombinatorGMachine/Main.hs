module Main where

import qualified LC
import qualified Compiler.LC
import qualified Machine
import Control.Monad.ST (runST)
import System.Environment (getArgs)
import Control.Monad (when)


data Config = Config
  { debugMode :: Bool
  , showSource :: Bool
  }


debug :: LC.Expr -> IO ()
debug expr = do
  let desugared = Compiler.LC.desugar expr
  let graph = runST do (Machine.toGraph . Compiler.LC.compile $ expr) >>= Machine.dump
  putStrLn (show desugared)
  putStrLn graph


eval :: Config -> LC.Expr -> String -> IO ()
eval config expr src = do
  let comb = Compiler.LC.compile expr
  let out = runST do
        ref <- Machine.toGraph comb
        Machine.normal ref >>= Machine.dump
  when (debugMode config) do debug expr
  when (showSource config) do putStrLn src
  putStrLn src
  putStrLn out


main :: IO ()
main = do
  args <- getArgs
  let config =
        Config
          ("--debug" `elem` args)
          ("--show-source" `elem` args)
  let stripped = filter (not . (`elem` ["--debug", "--show-source"])) args
  case stripped of
    path:_ -> LC.run path (eval config)
    _ -> fail "Usage: combinator-gmachine <filename> [--debug][--show-source]"
