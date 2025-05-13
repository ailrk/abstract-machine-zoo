module Main where

import qualified LC
import qualified Compiler.LC
import qualified Machine
import qualified Combinator
import System.Environment (getArgs)
import Control.Monad (when, void)
import Data.List (isPrefixOf)
import Text.Pretty.Simple (pPrintString)


data Config = Config
  { showSource :: Bool
  , showCore :: Bool
  , showCore1 :: Bool
  , showComb :: Bool
  , pretty :: Bool
  }


debug :: LC.Expr -> IO ()
debug expr = do
  let desugared = Compiler.LC.desugar expr
  graph <- (Machine.toGraph . Compiler.LC.compile $ expr) >>= Machine.dump
  putStrLn (show desugared)
  putStrLn graph


eval :: Config -> LC.Expr -> String -> IO ()
eval config expr src = do
  let print' = if pretty config then pPrintString else print
  when (showComb config) do
    graph <- (Machine.toGraph . Compiler.LC.compile $ expr) >>= Machine.dump
    print' graph
  when (showCore config) do
    let desugared = Compiler.LC.desugar expr
    print' (Combinator.displayCore desugared)
  when (showCore1 config) do
    let out = Compiler.LC.compile0
            . Compiler.LC.forceToplevelIO
            . Compiler.LC.desugar
            $ expr
    print' (Combinator.displayCore out)
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
          ("--pretty" `elem` args)
  let stripped = filter (not . ("--" `isPrefixOf`)) args
  case stripped of
    path:_ -> LC.run path (eval config)
    _ -> fail "Usage: combinator-gmachine <filename> [--debug][--show-source]"
