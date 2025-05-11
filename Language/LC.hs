module LC where

import Data.Void
import Data.Text (Text, pack, unpack)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import System.IO (readFile')


data OP
  = Add
  | Sub
  | Mul
  | Div
  | GT
  | GTE
  | LT
  | LTE
  | NOT
  | Eqv
  deriving (Eq, Show)


data Pattern
  = PVar Text
  | PInt Int
  deriving (Eq, Show)


data Expr
  = Var Text
  | Lam [Text] Expr
  | App Expr [Expr]
  | Let Text Expr Expr
  | LetRec Text Expr Expr
  | If Expr Expr Expr
  | IntLit Int
  | BinOp OP Expr Expr
  | UnOp OP Expr Expr
  | Match Expr [(Pattern, Expr)]
  deriving (Eq, Show)


type Parser = Parsec Void String

-- Lexer

sc :: Parser ()
sc = L.space space1 empty empty


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


symbol :: String -> Parser Text
symbol s = pack <$> L.symbol sc s


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


identifier :: Parser Text
identifier = pack <$> lexeme ((:) <$> letterChar <*> many alphaNumChar)


reserved :: [Text]
reserved = ["let", "rec", "in", "match", "with", "if", "then", "else"]


reservedWord :: String -> Parser ()
reservedWord w = lexeme (string w *> notFollowedBy alphaNumChar)


integer :: Parser Int
integer = lexeme L.decimal

-- Expressions

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable


operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ InfixL (BinOp Mul <$ symbol "*")
    , InfixL (BinOp Div <$ symbol "/") ]
  , [ InfixL (BinOp Add <$ symbol "+")
    , InfixL (BinOp Sub <$ symbol "-") ]
  , [ InfixL (BinOp Eqv <$ symbol "==") ]
  ]


pTerm :: Parser Expr
pTerm = choice
  [ try pLet
  , pLetRec
  , pIf
  , pMatch
  , pLam
  , try pApp
  , pFactor
  ]


pLam :: Parser Expr
pLam = do
  _ <- symbol "fn"
  args <- some (try identifier)
  _ <- symbol "=>"
  body <- pExpr
  return $ Lam args body


pApp :: Parser Expr
pApp = do
  f <- pFactor
  args <- some pFactor
  return $ App f args


pFactor :: Parser Expr
pFactor = choice
  [ IntLit <$> integer
  , Var <$> try (identifier >>= checkReserved)
  , parens pExpr
  ]
  where
    checkReserved s
      | s `elem` reserved = fail $ "reserved word: " ++ unpack s
      | otherwise = return s


pLet :: Parser Expr
pLet = do
  reservedWord "let"
  name <- identifier
  _ <- symbol "="
  val <- pExpr
  reservedWord "in"
  body <- pExpr
  return $ Let name val body


pLetRec :: Parser Expr
pLetRec = do
  reservedWord "let"
  reservedWord "rec"
  name <- identifier
  _ <- symbol "="
  body <- pExpr
  reservedWord "in"
  rest <- pExpr
  return $ LetRec name body rest


pIf :: Parser Expr
pIf = do
  reservedWord "if"
  cond <- pExpr
  reservedWord "then"
  tr <- pExpr
  reservedWord "else"
  fl <- pExpr
  return $ If cond tr fl


pMatch :: Parser Expr
pMatch = do
  reservedWord "match"
  expr <- pExpr
  reservedWord "with"
  alts <- some pCaseAlt
  return $ Match expr alts


pCaseAlt :: Parser (Pattern, Expr)
pCaseAlt = do
  _ <- symbol "|"
  pat <- pPattern
  _ <- symbol "->"
  body <- pExpr
  return (pat, body)


pPattern :: Parser Pattern
pPattern = (PInt <$> integer) <|> (PVar <$> identifier)


program :: Parser Expr
program = between sc eof pExpr


run :: FilePath -> (Expr -> IO ()) -> IO ()
run path eval = do
  putStrLn (">" ++ path)
  code <- readFile' path
  case parse program "" code of
    Right s -> eval s
    Left e -> putStrLn $ errorBundlePretty e
