module Main where
import System.Environment (getArgs)
import Data.List (isPrefixOf)
import qualified LC
import Control.Monad (when)
import qualified EvalApply.LC
import Control.Monad.Reader
import Data.Functor (void)


data Config = Config
  { showSource :: Bool
  }


eval :: Config -> LC.Expr -> String -> IO ()
eval config expr src = do
  when (showSource config) do
    putStrLn src
  void $ runReaderT
    (EvalApply.LC.eval expr)
    EvalApply.LC.initEnv



main :: IO ()
main = do
  args <- getArgs
  let config =
        Config
          ("--show-source" `elem` args)
  let stripped = filter (not . ("+" `isPrefixOf`))
               . filter (not . ("-" `isPrefixOf`))
               $ args
  case stripped of
    path:_ -> LC.run path (eval config)
    _ -> fail "Usage: combinator-gmachine <filename> [--debug][--show-source]"
