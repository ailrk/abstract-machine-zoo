module Main where

import qualified LC
import qualified Compiler.LC
import qualified Machine
import System.Environment (getArgs)
import Control.Monad (when, void)


data Config = Config
  { debugMode :: Bool
  , showSource :: Bool
  }


debug :: LC.Expr -> IO ()
debug expr = do
  let desugared = Compiler.LC.desugar expr
  graph <- (Machine.toGraph . Compiler.LC.compile $ expr) >>= Machine.dump
  putStrLn (show desugared)
  putStrLn graph


eval :: Config -> LC.Expr -> String -> IO ()
eval config expr src = do
  when (debugMode config) do debug expr
  when (showSource config) do putStrLn src
  let comb = Compiler.LC.compile expr
  ref <- Machine.toGraph comb
  void $ Machine.nf ref


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
