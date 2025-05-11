{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where

import qualified IMP
import qualified Compiler.IMP
import qualified Machine
import System.Environment (getArgs)
import Data.Foldable (traverse_)
import Control.Monad (when)


data Config = Config
  { debugMode :: Bool
  , showSource :: Bool
  }


debug :: Machine.Program -> IO ()
debug program = do
  let (Machine.Program (instrs, labels)) = program
  print labels
  traverse_ print (zip [0..] instrs)


eval :: Config -> IMP.Stmt -> String -> IO ()
eval config stmt src = do
  let program = Compiler.IMP.compile stmt
  when (showSource config) do putStrLn src
  when (debugMode config) do debug program
  Machine.start program


main :: IO ()
main = do
  args <- getArgs
  let config =
        Config
          ("--debug" `elem` args)
          ("--show-source" `elem` args)
  let stripped = filter (not . (`elem` ["--debug", "--show-source"])) args
  case stripped of
    path:_ -> IMP.run path (eval config)
    _ -> fail "Usage: stack-machine <filename> [--debug][--show-source]"
