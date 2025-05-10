module Main where

import qualified IMP
import qualified Compiler.IMP
import qualified Machine
import System.Environment (getArgs)


eval :: IMP.Stmt -> IO ()
eval stmt = do
  let program = Compiler.IMP.compile stmt
  Machine.start program


main :: IO ()
main = do
  args <- getArgs
  case args of
    path:_ -> IMP.run path eval
    _ -> fail "Usage: stack-machine <filename>"
