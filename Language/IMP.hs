module IMP where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import Data.Text (Text, pack)
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (LT, GT)
import System.IO (readFile')

data OP
  = Add
  | Sub
  | Mul
  | Div
  | Gt
  | Gte
  | Lt
  | Lte
  | Not
  | Eqv
  | Neq
  deriving (Eq, Show)


data Expr
  = Var Text
  | IntLit Int
  | BinOP OP Expr Expr
  | UnOP OP Expr
  deriving (Eq, Show)


data Stmt
  = Assign Text Expr
  | Seq [Stmt]
  | If Expr Stmt Stmt
  | While Expr Stmt
  | Call Text [Expr]
  | FuncDef Text [Text] Stmt
  | Print Expr
  deriving (Eq, Show)


type Parser = Parsec Void String


sc :: Parser ()
sc = L.space space1 empty empty


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


symbol :: String -> Parser Text
symbol s = pack <$> L.symbol sc s


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")


semi :: Parser Text
semi = symbol ";"


identifier :: Parser Text
identifier = pack <$> lexeme ((:) <$> letterChar <*> many alphaNumChar)


integer :: Parser Int
integer = lexeme L.decimal


expr :: Parser Expr
expr = makeExprParser term table
  where
    term =
          parens expr
      <|> UnOP Not <$ symbol "!" <*> term
      <|> IntLit <$> integer
      <|> Var <$> identifier

    table =
      [ [ Prefix (UnOP Not <$ symbol "!") ]
      , [ InfixL (BinOP Mul <$ symbol "*")
        , InfixL (BinOP Div <$ symbol "/") ]
      , [ InfixL (BinOP Add <$ symbol "+")
        , InfixL (BinOP Sub <$ symbol "-") ]
      , [ InfixN (BinOP Eqv <$ symbol "==")
        , InfixN (BinOP Neq <$ symbol "!=")
        , InfixN (BinOP Gt <$ symbol ">")
        , InfixN (BinOP Gte <$ symbol ">=")
        , InfixN (BinOP Lt <$ symbol "<")
        , InfixN (BinOP Lte <$ symbol "<=") ]
      ]


-- Statement parser
stmt :: Parser Stmt
stmt = choice
  [ Print <$ symbol "print" <*> expr <* semi
  , While <$ symbol "while" <*> expr <*> stmt
  , do
      e <- symbol "if" >> expr
      s1 <- stmt
      s2 <- symbol "else" *> stmt
      pure $ If e s1 s2
  , block
  , try $ Assign <$> identifier <* symbol ":=" <*> expr <* semi
  , try $ do
      _ <- symbol "func"
      name <- identifier
      params <- parens (identifier `sepBy` symbol ",")
      body <- block
      pure $ FuncDef name params body
  , Call <$> identifier <*> parens (expr `sepBy` symbol ",") <* semi
  ]


block :: Parser Stmt
block = Seq <$> braces (many stmt)


program :: Parser Stmt
program = between sc eof (Seq <$> many stmt)


run :: FilePath -> (Stmt -> IO ()) -> IO ()
run path eval = do
  putStrLn (">" ++ path)
  code <- readFile' path
  case parse program "" code of
    Right s -> eval s
    Left e -> putStrLn $ errorBundlePretty e
