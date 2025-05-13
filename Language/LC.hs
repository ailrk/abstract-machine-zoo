module LC where

-- A simple ML style untyped lambda calculus sugar.

import Data.Void
import Data.Text (Text, pack, unpack)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import System.IO (readFile')
import Data.Maybe (maybeToList)
import Control.Monad (join)
import OP (OP(..))


data Lit
  = IntLit Int
  | BoolLit Bool
  deriving (Eq, Show)


data Expr
  = Var Text
  | Lam [Text] Expr
  | App Expr [Expr]
  | Let Text [Text] Expr Expr
  | LetRec Text [Text] Expr Expr
  | If Expr Expr Expr
  | Lit Lit
  | BinOP OP Expr Expr
  | UnOP OP Expr
  | Do [Stmt]
  | List [Expr]
  deriving (Eq, Show)


data Stmt
  = LetBind Text Expr
  | Action Expr
  deriving (Eq, Show)


type Parser = Parsec Void String

-- Lexer

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockCommentNested "{-" "-}")


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


symbol :: String -> Parser Text
symbol s = pack <$> L.symbol sc s


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


identifier :: Parser Text
identifier = pack <$> lexeme ((:) <$> c1 <*> many c2)
  where
    c1 = letterChar <|> char '_' <|> char '\''
    c2 = alphaNumChar <|> char '_' <|> char '\''


reserved :: [Text]
reserved = ["let", "rec", "in", "match", "with", "if", "then", "else", "true", "false", "do", "<-", ";"]


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
  [ between (symbol "(") (symbol ")") pTerm'
  , pTerm'
  ]
  where
    pTerm' = choice
      [ UnOP Not <$ symbol "!" <*> pTerm
      , try pDo
      , try pLetRec
      , try pLet
      , try pIf
      , try pLam
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
  [ pList
  , try $ Lit <$> pLit
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
  params <- optional (some identifier) >>= pure . join . maybeToList
  _ <- symbol "="
  val <- pExpr
  reservedWord "in"
  body <- pExpr
  return $ Let name params val body


pLetRec :: Parser Expr
pLetRec = do
  reservedWord "let"
  reservedWord "rec"
  name <- identifier
  params <- optional (some identifier) >>= pure . join . maybeToList
  _ <- symbol "="
  body <- pExpr
  reservedWord "in"
  rest <- pExpr
  return $ LetRec name params body rest


pIf :: Parser Expr
pIf = do
  reservedWord "if"
  cond <- pExpr
  reservedWord "then"
  tr <- pExpr
  reservedWord "else"
  fl <- pExpr
  return $ If cond tr fl


pDo :: Parser Expr
pDo = do
  reservedWord "do"
  stmts <- between (symbol "{") (symbol "}") (sepBy pStmt (symbol ";"))
  pure (Do stmts)


pStmt :: Parser Stmt
pStmt = do
  stmt <- try pLetBind <|> pAction
  pure stmt


pLetBind :: Parser Stmt
pLetBind = do
  var <- identifier
  _ <- symbol "<-"
  expr <- pExpr
  pure (LetBind var expr)


pAction :: Parser Stmt
pAction = Action <$> pExpr


pList :: Parser Expr
pList = List <$> between (symbol "[") (symbol "]") (pExpr `sepBy` symbol ",")


program :: Parser Expr
program = between sc eof pExpr


run :: FilePath -> (Expr -> String -> IO ()) -> IO ()
run path eval = do
  putStrLn ("\x1b[32;1m>"++ path ++ "\x1b[0m")
  code <- readFile' path
  case parse program "" code of
    Right s -> eval s code
    Left e -> putStrLn $ errorBundlePretty e
