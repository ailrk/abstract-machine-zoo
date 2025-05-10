{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where

import qualified IMP
import qualified Compiler.IMP
import qualified Machine
import System.Environment (getArgs)
import Data.Foldable (traverse_)


data Mode
  = Normal
  | Debug


debug :: Machine.Program -> IO ()
debug program = do
  let (Machine.Program (instrs, labels)) = program
  print labels
  traverse_ print (zip [0..] instrs)


eval :: Mode -> IMP.Stmt -> IO ()
eval mode stmt = do
  let program = Compiler.IMP.compile stmt
  case mode of
    Debug -> debug program
    Normal -> pure ()
  Machine.start program


main :: IO ()
main = do
  args <- getArgs
  let mode = if "--debug" `elem` args then Debug else Normal
  let stripped = filter (/= "--debug") args
  case stripped of
    path:_ -> IMP.run path (eval mode)
    _ -> fail "Usage: stack-machine <filename> [--debug]"
