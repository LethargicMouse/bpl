{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative (Alternative (..), liftA2, many, optional)
import Control.Monad.State
  ( MonadState,
    StateT (runStateT),
    evalStateT,
    get,
    gets,
    lift,
    modify,
    put,
  )
import Control.Monad.Writer (execWriter, tell)
import Data.Char (isAlpha, isDigit)
import Data.List (nub)
import Data.Maybe (maybeToList)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith)
import System.Process.Typed (runProcess)

data Pos = Pos
  { posLine :: Int,
    posSymbol :: Int
  }
  deriving (Eq)

instance Ord Pos where
  pa <= pb
    | posLine pa == posLine pb = posSymbol pa <= posSymbol pb
    | otherwise = posLine pa <= posLine pb

instance Show Pos where
  show Pos {..} = "line " ++ show posLine ++ ", symbol " ++ show posSymbol

data Token
  = Fn
  | NameTok String
  | ParL
  | Colon
  | ParR
  | CurL
  | Point
  | EqTok
  | IntTok Int
  | StrTok String
  | Semicolon
  | CurR
  | Comma
  | EofTok
  | IfTok
  | Eq2
  deriving (Eq, Show)

data ParserState = ParserState
  { psSource :: [(Pos, Token)],
    psError :: ParseError
  }
  deriving (Show)

data ParseError = ParseError
  { peMsgs :: [String],
    pePos :: Pos
  }

instance Semigroup ParseError where
  ea <> eb
    | pePos ea < pePos eb = eb
    | pePos ea == pePos eb =
        ParseError {pePos = pePos ea, peMsgs = peMsgs ea ++ peMsgs eb}
    | otherwise = ea

instance Monoid ParseError where
  mempty = ParseError {peMsgs = [], pePos = Pos {posLine = 0, posSymbol = 0}}

instance Show ParseError where
  show ParseError {..} =
    "parse: at " ++ show pePos ++ ":" ++ concatMap ("\n  " ++) (nub peMsgs)

newtype Parser a = Parser
  { unParser :: StateT ParserState (Either ParseError) a
  }
  deriving (Functor, Applicative, Monad, MonadState ParserState)

instance Alternative Parser where
  empty = Parser . lift . Left =<< gets psError
  pa <|> pb =
    get >>= \state ->
      case (runStateT (unParser pa) state, runStateT (unParser pb) state) of
        (Left e1, Left e2) -> addError e1 >> addError e2 >> empty
        (Left e, Right (b, ps)) -> put ps >> addError e >> pure b
        (Right (a, ps), _) -> put ps >> pure a

data TypeName
  = TypeName String
  | Poiner TypeName
  deriving (Show)

data FunctionHeader = FunctionHeader
  { functionName :: String,
    functionArgs :: [(TypeName, String)]
  }
  deriving (Show)

data Binary
  = Equal
  deriving (Show)

data Literal
  = IntLit Int
  | StrLit String
  | UnitLit
  deriving (Show)

data Expr
  = VarExpr String
  | FieldExpr String Expr
  | BinExpr Expr Binary Expr
  | LitExpr Literal
  | CallExpr String [Expr]
  deriving (Show)

data Statement
  = EvalStatement Expr
  | IfStatement Expr Block
  deriving (Show)

data GenState = GenState {gsProgram :: Program, gsBreakn :: Int}

data Block = Block
  { blockStatements :: [Statement],
    blockReturn :: Expr
  }
  deriving (Show)

data Function = Function
  { functionHeader :: FunctionHeader,
    functionBody :: Block
  }
  deriving (Show)

data Type = Int deriving (Show)

data Program = Program
  { programFunctions :: [Function],
    programTypes :: [Type]
  }
  deriving (Show)

todo :: String -> a
todo s = error ("todo: " ++ s)

addError :: ParseError -> Parser ()
addError pe = modify $ \ps@ParserState {..} -> ps {psError = psError <> pe}

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      src <- readFile path
      tokens <- either die return (tokenize src)
      program <-
        either
          (die . show)
          return
          (runParser parseProgram $ parserSource tokens)
      writeFile "out.asm" (genAsm program)
      exitWith
        =<< runProcess "nasm -felf64 -g out.asm && ld -o out out.o && ./out"
    _ -> die "incorrect arguments set\n  usage: bpl path/to/file"

tokenize :: String -> Either String [(Pos, Token)]
tokenize = f 1 1
  where
    f l s [] = Right [(Pos {posLine = l, posSymbol = s}, EofTok)]
    f l s src =
      case src of
        'f' : 'n' : o -> push Fn l s 2 o
        'i' : 'f' : o -> push IfTok l s 2 o
        ' ' : o -> f l (s + 1) o
        '(' : o -> push ParL l s 1 o
        ':' : o -> push Colon l s 1 o
        ')' : o -> push ParR l s 1 o
        '{' : o -> push CurL l s 1 o
        '\n' : o -> f (l + 1) 1 o
        '.' : o -> push Point l s 1 o
        '=' : '=' : o -> push Eq2 l s 2 o
        '=' : o -> push EqTok l s 1 o
        ';' : o -> push Semicolon l s 1 o
        '}' : o -> push CurR l s 1 o
        '\"' : o ->
          let (str, other) = span (/= '\"') o
           in case other of
                '\"' : other2 -> push (StrTok str) l s (length str + 2) other2
                _ ->
                  Left $
                    "tokenize: at "
                      ++ show Pos {posLine = l, posSymbol = s}
                      ++ ": unclosed string"
        a : o
          | isAlpha a ->
              let (name, other) = span isAlpha (a : o)
               in push (NameTok name) l s (length name) other
          | isDigit a ->
              let (num, other) = span isDigit (a : o)
               in push (IntTok $ read num) l s (length num) other
        a : _ ->
          Left $
            "tokenize: at "
              ++ show Pos {posLine = l, posSymbol = s}
              ++ ": unexpected symbol: "
              ++ show a
    push t l s n o = ((Pos {posLine = l, posSymbol = s}, t) :) <$> f l (s + n) o

parseProgram :: Parser Program
parseProgram = do
  programFunctions <- many parseFunction
  expectToken EofTok
  let programTypes = []
  return Program {..}

parseFunction :: Parser Function
parseFunction = Function <$> parseFunctionHeader <*> parseBlock

parseBlock :: Parser Block
parseBlock = do
  expectToken CurL
  blockStatements <- many parseStatement
  blockReturn <- parseExpr <|> pure (LitExpr UnitLit)
  expectToken CurR
  return Block {..}

parseExpr :: Parser Expr
parseExpr = do
  ubins <- many $ (,) <$> un <*> parseBinary
  lastu <- un
  return $ foldr prioritize lastu ubins
  where
    un = do
      f <-
        ( LitExpr
            <$> parseLiteral
          )
          <|> call
          <|> ( VarExpr
                  <$> parseName "variable name"
              )
      fieldize f
    call = do
      name <- parseName "function name"
      expectToken ParL
      args <-
        liftA2
          (++)
          (maybeToList <$> optional parseExpr)
          (many $ expectToken Comma *> parseExpr)
      expectToken ParR
      return $ CallExpr name args
    fieldize expr = do
      f <- optional field
      case f of
        Nothing -> return expr
        Just (VarExpr v) ->
          fieldize (FieldExpr v expr)
        Just (CallExpr callName callArgs) ->
          fieldize (CallExpr callName $ expr : callArgs)
        _ -> error "incorrect field parsed"
    field = do
      expectToken Point
      call <|> (VarExpr <$> parseName "field name")
    prioritize (a, bin1) e@(BinExpr b bin2 c)
      | binPrior bin1 < binPrior bin2 = BinExpr a bin1 e
      | otherwise = BinExpr (prioritize (a, bin1) b) bin2 c
    prioritize (a, bin) b = BinExpr a bin b

binPrior :: Binary -> Int
binPrior Equal = 3

parseBinary :: Parser Binary
parseBinary = Equal <$ expectToken Eq2

parseLiteral :: Parser Literal
parseLiteral =
  (IntLit <$> parseInt)
    <|> (StrLit <$> parseStr)
    <|> do
      expectToken ParL
      expectToken ParR
      return UnitLit

parseStr :: Parser String
parseStr = do
  src <- gets psSource
  case src of
    (_, StrTok s) : other ->
      setParserSource other >> return s
    _ -> failParse "expected string literal"

parseInt :: Parser Int
parseInt = do
  src <- gets psSource
  case src of
    (_, IntTok i) : other ->
      setParserSource other >> return i
    _ -> failParse "expected integer"

parseStatement :: Parser Statement
parseStatement = parseIf <|> EvalStatement <$> parseExpr <* expectToken Semicolon
  where
    parseIf = do
      expectToken IfTok
      condition <- parseExpr
      IfStatement condition <$> parseBlock

parseFunctionHeader :: Parser FunctionHeader
parseFunctionHeader = do
  expectToken Fn
  functionName <- parseName "function name"
  expectToken ParL
  functionArgs <- liftA2 (:) parseArg $ many (expectToken Comma >> parseArg)
  expectToken ParR
  return FunctionHeader {..}
  where
    parseArg = do
      argName <- parseName "argument name"
      expectToken Colon
      argType <- parseTypeName
      return (argType, argName)

parseTypeName :: Parser TypeName
parseTypeName = TypeName <$> parseName "type name"

parseName :: String -> Parser String
parseName what = do
  src <- gets psSource
  case src of
    (_, NameTok s) : other ->
      setParserSource other >> return s
    _ -> failParse $ "expected " ++ what

expectToken :: Token -> Parser ()
expectToken t = do
  src <- gets psSource
  case src of
    (_, curTok) : other
      | curTok == t -> setParserSource other
      | otherwise -> failParse $ "expected " ++ show t
    _ -> error "out of tokens"

setParserSource :: [(Pos, Token)] -> Parser ()
setParserSource other = modify (\ps -> ps {psSource = other})

failParse :: String -> Parser a
failParse err = do
  pePos <- gets (fst . head . psSource)
  addError ParseError {peMsgs = [err], ..}
  empty

parserSource :: [(Pos, Token)] -> ParserState
parserSource tokens = ParserState {psSource = tokens, psError = mempty}

runParser :: Parser a -> ParserState -> Either ParseError a
runParser = evalStateT . unParser

genAsm :: Program -> String
genAsm p = execWriter $ runStateT genProgram $ genState p
  where
    genProgram = do
      lift $
        tell
          "segment .text\n\
          \global _start\n\
          \_start:\n\
          \call main\n\
          \mov rax, 60\n\
          \mov rdi, 0\n\
          \syscall\n"

genState :: Program -> GenState
genState = todo "genState"

die :: String -> IO a
die err = do
  putStrLn err
  exitFailure
