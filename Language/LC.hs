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
  | Gt
  | Gte
  | Lt
  | Lte
  | Not
  | Eqv
  | Neq
  deriving (Eq, Show)

data Lit
  = IntLit Int
  | BoolLit Bool
  deriving (Eq, Show)


data Expr
  = Var Text
  | Lam [Text] Expr
  | App Expr [Expr]
  | Let Text Expr Expr
  | LetRec Text Expr Expr
  | If Expr Expr Expr
  | Lit Lit
  | BinOP OP Expr Expr
  | UnOP OP Expr
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
reserved = ["let", "rec", "in", "match", "with", "if", "then", "else", "true", "false"]


reservedWord :: String -> Parser ()
reservedWord w = lexeme (string w *> notFollowedBy alphaNumChar)


integer :: Parser Int
integer = lexeme L.decimal

-- Expressions

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable


operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ InfixL (BinOP Mul <$ symbol "*")
    , InfixL (BinOP Div <$ symbol "/") ]
  , [ InfixL (BinOP Add <$ symbol "+")
    , InfixL (BinOP Sub <$ symbol "-") ]
  , [ InfixL (BinOP Eqv <$ symbol "==")
    , InfixL (BinOP Neq <$ symbol "!=")
    , InfixN (BinOP Gt <$ symbol ">")
    , InfixN (BinOP Gte <$ symbol ">=")
    , InfixN (BinOP Lt <$ symbol "<")
    , InfixN (BinOP Lte <$ symbol "<=")
    ]
  ]


pTerm :: Parser Expr
pTerm = choice
  [ UnOP Not <$ symbol "!" <*> pTerm
  , try pLet
  , pLetRec
  , pIf
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


pLit :: Parser Lit
pLit = choice
  [ IntLit <$> integer
  , BoolLit <$> do
      bool <- choice [symbol "true", symbol "false"]
      case bool of
        "true" -> pure True
        "false" -> pure False
        _ -> fail "invalid boolean literal"
  ]


pFactor :: Parser Expr
pFactor = choice
  [ try $ Lit <$> pLit
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


program :: Parser Expr
program = between sc eof pExpr


run :: FilePath -> (Expr -> IO ()) -> IO ()
run path eval = do
  putStrLn (">" ++ path)
  code <- readFile' path
  case parse program "" code of
    Right s -> eval s
    Left e -> putStrLn $ errorBundlePretty e
