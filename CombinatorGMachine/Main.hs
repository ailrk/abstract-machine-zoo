module Main where

import qualified LC
import qualified Compiler.LC
import qualified Machine
import System.Environment (getArgs)
import Control.Monad (when, void)
import Data.List (isPrefixOf)


data Config = Config
  { showSource :: Bool
  , showCore :: Bool
  , showCore1 :: Bool
  , showComb :: Bool
  }


debug :: LC.Expr -> IO ()
debug expr = do
  let desugared = Compiler.LC.desugar expr
  graph <- (Machine.toGraph . Compiler.LC.compile $ expr) >>= Machine.dump
  putStrLn (show desugared)
  putStrLn graph


eval :: Config -> LC.Expr -> String -> IO ()
eval config expr src = do
  when (showComb config) do
    graph <- (Machine.toGraph . Compiler.LC.compile $ expr) >>= Machine.dump
    putStrLn graph
  when (showCore config) do
    let desugared = Compiler.LC.desugar expr
    putStrLn (show desugared)
  when (showCore1 config) do
    let out = Compiler.LC.compile0
            . Compiler.LC.forceToplevelIO
            . Compiler.LC.desugar
            $ expr
    putStrLn (show out)
  when (showSource config) do
    putStrLn src
  let comb = Compiler.LC.compile expr
  ref <- Machine.toGraph comb
  void $ Machine.nf ref


main :: IO ()
main = do
  args <- getArgs
  let config =
        Config
          ("--show-source" `elem` args)
          ("--show-core" `elem` args)
          ("--show-core1" `elem` args)
          ("--show-comb" `elem` args)
  let stripped = filter (not . ("--" `isPrefixOf`)) args
  case stripped of
    path:_ -> LC.run path (eval config)
    _ -> fail "Usage: combinator-gmachine <filename> [--debug][--show-source]"
