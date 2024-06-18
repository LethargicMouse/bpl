{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative (Alternative (..), liftA2, many, optional)
import Control.Arrow (first)
import Control.Monad (forM, forM_, when)
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
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Char (isAlpha, isDigit)
import Data.List (find, nub, (\\))
import qualified Data.Map as M
import Data.Maybe (isJust, maybeToList)
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
  | Ampersand
  | StructTok
  | Lang
  | Rang
  | TypeTok
  | SqL
  | SqR
  | Neq
  | Plus
  | LetTok
  | WhileTok
  deriving (Eq)

instance Show Token where
  show Fn = "'fn'"
  show (NameTok s) = "'" ++ s ++ "'"
  show ParL = "'('"
  show Colon = "':'"
  show ParR = "')'"
  show CurL = "'{'"
  show Point = "'.'"
  show EqTok = "'='"
  show (IntTok i) = show i
  show (StrTok s) = show s
  show Semicolon = "';'"
  show CurR = "'}'"
  show Comma = "','"
  show EofTok = "<eof>"
  show IfTok = "'if'"
  show Eq2 = "'=='"
  show Ampersand = "'&'"
  show StructTok = "'struct'"
  show Lang = "'<'"
  show Rang = "'>'"
  show TypeTok = "'type'"
  show SqL = "'['"
  show SqR = "']'"
  show Neq = "'!='"
  show Plus = "'+'"
  show LetTok = "'let'"
  show WhileTok = "'while'"

data ParserState = ParserState
  { psSource :: [(Pos, Token)],
    psError :: ParseError,
    psTarget :: [String],
    psSrcLines :: [String]
  }
  deriving (Show)

data ParseError = ParseError
  { peMsgs :: [[String]],
    pePos :: Pos,
    peLine :: String
  }
  deriving (Show)

instance Semigroup ParseError where
  ea <> eb
    | pePos ea < pePos eb = eb
    | pePos ea == pePos eb =
        ParseError
          { pePos = pePos ea,
            peLine = peLine ea,
            peMsgs = peMsgs ea ++ peMsgs eb
          }
    | otherwise = ea

instance Monoid ParseError where
  mempty =
    ParseError
      { peMsgs = [],
        pePos = Pos {posLine = 0, posSymbol = 0},
        peLine = ""
      }

{-instance Show ParseError where
show ParseError {..} =
  "parse: at "
    ++ show pePos
    ++ ":\n"
    ++ show (trie $ nub peMsgs)
    ++ "\non line:\n"
    ++ peLine
    ++ "\n"
    ++ replicate (posSymbol pePos - 1) ' '
    ++ "^"-}

data Trie = Trie
  { trieVal :: String,
    trieChildren :: M.Map String Trie
  }

instance Show Trie where
  show t = tail $ concatMap (f 0 . snd) (M.toAscList $ trieChildren t)
    where
      f :: Int -> Trie -> String
      f n Trie {..} =
        "\n"
          ++ replicate n ' '
          ++ trieVal
          ++ concatMap (f (n + 2) . snd) (M.toAscList trieChildren)

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
  = TypeName String [TypeName]
  | Pointer TypeName
  deriving (Show, Eq)

data FunctionHeader = FunctionHeader
  { functionName :: String,
    functionArgs :: [(String, TypeName)]
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
  | ElemExpr Expr Expr
  | BinExpr Expr Binary Expr
  | LitExpr Literal
  | CallExpr String [Expr]
  deriving (Show)

data InferredExpr
  = IVarExpr String
  | IFieldExpr String InferredExpr
  | IElemExpr InferredExpr InferredExpr
  | IBinExpr InferredExpr Binary InferredExpr
  | ILitExpr Literal
  | ICallExpr String [InferredExpr]

data Statement
  = EvalStatement Expr
  | IfStatement Expr Block
  | WhileStatement Expr Block
  | DefStatement String TypeName Expr
  deriving (Show)

data InferredStatement
  = IEvalStatement InferredExpr
  | IIfStatement InferredExpr InferredBlock
  | IWhileStatement InferredExpr InferredBlock
  | IDefStatement String TypeName InferredExpr

data Var = Var
  { varPos :: Int,
    varSize :: Int,
    varType :: TypeName
  }

data GenState = GenState
  { gsLoopn :: Int,
    gsBreakn :: Int,
    gsRsp :: Int,
    gsVars :: M.Map String Var,
    gsConsts :: [String]
  }

type CodeGen a = StateT GenState (Writer String) a

data Block = Block
  { blockStatements :: [Statement],
    blockReturn :: Expr
  }
  deriving (Show)

data InferredBlock = IBlock
  { iblockStatements :: [InferredStatement],
    iblockReturn :: InferredExpr
  }

data InferState = InferState
  {
  }

data Function = Function
  { functionHeader :: FunctionHeader,
    functionBody :: Block
  }
  deriving (Show)

data InferredFunction = IFunction
  { ifunctionHeader :: FunctionHeader,
    ifunctionBody :: InferredBlock
  }

data TopLevel
  = TopFunction Function
  | TopType Type

data Struct = Struct
  { structName :: String,
    structFields :: [(String, TypeName)],
    structGenerics :: [String]
  }
  deriving (Show)

data Type
  = Int
  | StructType Struct
  | TypeAlias String TypeName
  deriving (Show)

data Program = Program
  { programFunctions :: [Function],
    programTypes :: [Type]
  }
  deriving (Show)

data InferredProgram = IProgram
  { iprogramFunctions :: [InferredFunction],
    iprogramTypes :: [Type]
  }

trie :: [[String]] -> Trie
trie = foldr insTrie Trie {trieVal = "", trieChildren = M.empty}

insTrie :: [String] -> Trie -> Trie
insTrie [] t = t
insTrie (a : as) t = case M.lookup a (trieChildren t) of
  Nothing ->
    t
      { trieChildren =
          M.insert
            a
            ( insTrie as $
                Trie
                  { trieVal = a,
                    trieChildren = M.empty
                  }
            )
            (trieChildren t)
      }
  Just t2 ->
    t
      { trieChildren = M.insert a (insTrie as t2) (trieChildren t)
      }

todo :: String -> a
todo s = error ("todo: " ++ s)

addError :: ParseError -> Parser ()
addError pe = modify $ \ps@ParserState {..} -> ps {psError = psError <> pe}

parseFor :: String -> Parser a -> Parser a
parseFor s p = addTarget s *> p <* popTarget

addTarget :: String -> Parser ()
addTarget s = modify (\ps -> ps {psTarget = s : psTarget ps})

popTarget :: Parser ()
popTarget = modify (\ps -> ps {psTarget = tail $ psTarget ps})

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
          (runParser parseProgram $ parserSource tokens src)
      iprogram <- either die return (evalStateT (infer program) initInferState)
      writeFile "out.asm" (genAsm iprogram)
      exitWith
        =<< runProcess "nasm -felf64 -g out.asm && ld -o out out.o"
    _ -> die "incorrect arguments set\n  usage: bpl path/to/file"

initInferState :: InferState
initInferState = todo "initInferState"

infer :: Program -> StateT InferState (Either String) InferredProgram
infer Program {..} = do
  iprogramFunctions <- mapM inferFn programFunctions
  return IProgram {iprogramTypes = programTypes, ..}
  where
    inferFn Function {..} = do
      ifunctionBody <- inferBlock functionBody
      return IFunction {ifunctionHeader = functionHeader, ..}

    inferBlock Block {..} = do
      iblockStatements <- mapM inferStatement blockStatements
      iblockReturn <- inferExpr blockReturn
      return IBlock {..}

    inferStatement (EvalStatement e) = IEvalStatement <$> inferExpr e
    inferStatement (IfStatement condition body) = do
      icondition <- inferExpr condition
      ibody <- inferBlock body
      return (IIfStatement icondition ibody)
    inferStatement (DefStatement n t e) =
      IDefStatement n t <$> inferExpr e
    inferStatement (WhileStatement c b) = do
      ic <- inferExpr c
      ib <- inferBlock b
      return (IWhileStatement ic ib)

    inferExpr (VarExpr v) = pure (IVarExpr v)
    inferExpr (FieldExpr f e) = IFieldExpr f <$> inferExpr e
    inferExpr (BinExpr a bin b) = do
      ia <- inferExpr a
      ib <- inferExpr b
      return (IBinExpr ia bin ib)
    inferExpr (LitExpr lit) = pure (ILitExpr lit)
    inferExpr (CallExpr s args) = ICallExpr s <$> mapM inferExpr args
    inferExpr (ElemExpr i e) = do
      ii <- inferExpr i
      ie <- inferExpr e
      return (IElemExpr ii ie)

tokenize :: String -> Either String [(Pos, Token)]
tokenize = f 1 1
  where
    f l s [] = Right [(Pos {posLine = l, posSymbol = s}, EofTok)]
    f l s src =
      case src of
        'f' : 'n' : o -> push Fn l s 2 o
        'i' : 'f' : o -> push IfTok l s 2 o
        's' : 't' : 'r' : 'u' : 'c' : 't' : o ->
          push StructTok l s 6 o
        't' : 'y' : 'p' : 'e' : o ->
          push TypeTok l s 4 o
        'l' : 'e' : 't' : o -> push LetTok l s 3 o
        'w' : 'h' : 'i' : 'l' : 'e' : o ->
          push WhileTok l s 5 o
        ' ' : o -> f l (s + 1) o
        '(' : o -> push ParL l s 1 o
        ':' : o -> push Colon l s 1 o
        ')' : o -> push ParR l s 1 o
        '<' : o -> push Lang l s 1 o
        '>' : o -> push Rang l s 1 o
        '[' : o -> push SqL l s 1 o
        ']' : o -> push SqR l s 1 o
        '{' : o -> push CurL l s 1 o
        '\n' : o -> f (l + 1) 1 o
        '.' : o -> push Point l s 1 o
        '!' : '=' : o -> push Neq l s 2 o
        '=' : '=' : o -> push Eq2 l s 2 o
        '=' : o -> push EqTok l s 1 o
        ';' : o -> push Semicolon l s 1 o
        '}' : o -> push CurR l s 1 o
        ',' : o -> push Comma l s 1 o
        '&' : o -> push Ampersand l s 1 o
        '+' : o -> push Plus l s 1 o
        '-' : '-' : o -> f l s (dropWhile (/= '\n') o)
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
  (programTypes, programFunctions) <- foldr sieve ([Int], []) <$> many parseTopLevel
  expectToken EofTok
  return Program {..}
  where
    sieve (TopFunction f) (ts, fs) = (ts, f : fs)
    sieve (TopType t) (ts, fs) = (t : ts, fs)

parseTopLevel :: Parser TopLevel
parseTopLevel =
  parseFor "top level decl" $
    (TopFunction <$> parseFunction)
      <|> (TopType <$> parseType)

parseType :: Parser Type
parseType =
  parseFor "type" $
    (StructType <$> parseStruct)
      <|> parseFor
        "alias"
        ( do
            expectToken TypeTok
            name <- parseName "type alias"
            expectToken EqTok
            TypeAlias name
              <$> parseTypeName
              <* expectToken Semicolon
        )

parseStruct :: Parser Struct
parseStruct = parseFor "struct" $ do
  expectToken StructTok
  structName <- parseName "struct name"
  structGenerics <-
    parseFor "generics" $
      ( expectToken Lang
          *> liftA2
            (:)
            (parseName "generic name")
            (many $ expectToken Comma *> parseName "generic name")
          <* expectToken Rang
      )
        <|> pure []
  expectToken CurL
  structFields <- liftA2 (:) parseField (many $ expectToken Comma *> parseField)
  expectToken Comma <|> pure ()
  expectToken CurR
  return Struct {..}
  where
    parseField = parseFor "field" $ do
      fieldName <- parseName "field name"
      expectToken Colon
      fieldType <- parseTypeName
      return (fieldName, fieldType)

parseFunction :: Parser Function
parseFunction =
  parseFor "function" $
    Function
      <$> parseFunctionHeader
      <*> parseBlock

parseBlock :: Parser Block
parseBlock = parseFor "block" $ do
  expectToken CurL
  blockStatements <- many parseStatement
  blockReturn <- parseExpr <|> pure (LitExpr UnitLit)
  expectToken CurR
  return Block {..}

parseExpr :: Parser Expr
parseExpr = parseFor "expr" $ do
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

    call = parseFor "function call" $ do
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
        Just (ElemExpr i _) -> fieldize (ElemExpr i expr)
        _ -> error "incorrect field parsed"

    field =
      parseFor "field" (expectToken Point *> call)
        <|> parseFor
          "method"
          ( expectToken Point
              >> VarExpr <$> parseName "field name"
          )
        <|> parseFor
          "array elem"
          ( expectToken SqL
              >> flip ElemExpr undefined
                <$> parseExpr
                <* expectToken SqR
          )

    prioritize (a, bin1) e@(BinExpr b bin2 c)
      | binPrior bin1 < binPrior bin2 = BinExpr a bin1 e
      | otherwise = BinExpr (prioritize (a, bin1) b) bin2 c
    prioritize (a, bin) b = BinExpr a bin b

binPrior :: Binary -> Int
binPrior Equal = 3

parseBinary :: Parser Binary
parseBinary = parseFor "binary" $ Equal <$ expectToken Eq2

parseLiteral :: Parser Literal
parseLiteral =
  parseFor "literal" $
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
parseStatement =
  parseFor "statement" $
    parseFor
      "if"
      ( do
          expectToken IfTok
          condition <- parseExpr
          IfStatement condition <$> parseBlock
      )
      <|> parseFor
        "while"
        ( do
            expectToken WhileTok
            condition <- parseExpr
            WhileStatement condition <$> parseBlock
        )
      <|> parseFor
        "var decl"
        ( do
            expectToken LetTok
            name <- parseName "variable name"
            expectToken Colon
            t <- parseTypeName
            expectToken EqTok
            expr <- parseExpr
            expectToken Semicolon
            return (DefStatement name t expr)
        )
      <|> ( EvalStatement
              <$> parseExpr
              <* expectToken Semicolon
          )

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
      return (argName, argType)

parseTypeName :: Parser TypeName
parseTypeName =
  do
    name <- parseName "type name"
    generics <-
      expectToken Lang
        *> liftA2
          (:)
          parseTypeName
          ( many $
              expectToken Comma *> parseTypeName
          )
        <* expectToken Rang
          <|> pure []
    return (TypeName name generics)
    <|> do
      expectToken Ampersand
      Pointer <$> parseTypeName

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
  target <- gets psTarget
  peLine <- gets (\ps -> psSrcLines ps !! (posLine pePos - 1))
  addError
    ParseError
      { peMsgs =
          [ map
              (\t -> "for " ++ t ++ ":")
              (reverse target)
              ++ [err]
          ],
        ..
      }
  empty

parserSource :: [(Pos, Token)] -> String -> ParserState
parserSource tokens src =
  ParserState
    { psSource = tokens,
      psError = mempty,
      psTarget = [],
      psSrcLines = lines src
    }

runParser :: Parser a -> ParserState -> Either ParseError a
runParser = evalStateT . unParser

genAsm :: InferredProgram -> String
genAsm program =
  execWriter $ runStateT genProgram initGenState
  where
    genProgram :: CodeGen ()
    genProgram = do
      lift $
        tell
          "segment .text\n\
          \global _start\n\
          \_start:\n\
          \call main\n\
          \mov rax, 60\n\
          \mov rdi, 0\n\
          \syscall\n\
          \write:\n\
          \push rbp\n\
          \mov rbp, rsp\n\
          \mov rdx, [rbp + 16]\n\
          \mov rsi, [rbp + 24]\n\
          \mov rdi, [rbp + 32]\n\
          \mov rax, 1\n\
          \syscall\n\
          \pop rbp\n\
          \ret\n\
          \exit:\n\
          \push rbp\n\
          \mov rbp, rsp\n\
          \mov rdi, [rbp + 16]\n\
          \mov rax, 60\n\
          \syscall\n"
      mapM_ genFunction (iprogramFunctions program)

    genFunction :: InferredFunction -> CodeGen ()
    genFunction IFunction {ifunctionHeader = FunctionHeader {..}, ..} = do
      lift $ tell (functionName ++ ":\npush rbp\nmov rbp, rsp\n")
      forM_ (zip functionArgs [2 ..]) $ \((arg, argt), i) -> do
        modify
          ( \gs ->
              gs
                { gsVars =
                    M.insert
                      arg
                      Var
                        { varPos = (-8) * i,
                          varSize = typeNameSize argt,
                          varType = argt
                        }
                      (gsVars gs)
                }
          )
      mapM_ genStatement (iblockStatements ifunctionBody)
      msz <- genExpr (iblockReturn ifunctionBody)
      case msz of
        Just _ -> todo "big value return"
        Nothing -> pure ()
      rsp <- gets gsRsp
      lift $ tell ("add rsp, " ++ show rsp ++ "\npop rbp\nret\n")
      consts <- gets gsConsts
      forM_ (zip [0 :: Int ..] $ reverse consts) $ \(i, c) -> do
        lift $ tell (".C" ++ show i ++ ": db \"" ++ str2asm c ++ "\"\n")
      put initGenState

    str2asm :: String -> String
    str2asm [] = []
    str2asm ('\\' : 'n' : o) = "\", 10, \"" ++ str2asm o
    str2asm (a : o) = a : str2asm o

    genStatement :: InferredStatement -> CodeGen ()
    genStatement (IEvalStatement e) = do
      msz <- genExpr e
      case msz of
        Just sz -> do
          modify (\gs -> gs {gsRsp = gsRsp gs - sz})
          lift $ tell ("add rsp, " ++ show sz ++ "\n")
        Nothing -> pure ()
    genStatement (IIfStatement e b) = do
      msz <- genExpr e
      when (isJust msz) (error "expected boolean for if")
      breakn <- gets gsBreakn
      lift $ tell ("cmp rax, 0\nje .B" ++ show breakn ++ "\n")
      mapM_ genStatement (iblockStatements b)
      lift $ tell (".B" ++ show breakn ++ ":\n")
      modify (\gs -> gs {gsBreakn = breakn + 1})
    genStatement (IDefStatement n t e) = do
      msz <- genExpr e
      sz <- case msz of
        Nothing -> do
          modify (\gs -> gs {gsRsp = gsRsp gs + 8})
          lift $ tell "sub rsp, 8\nmov [rsp], rax\n"
          return 8
        Just sz -> return sz
      rsp <- gets gsRsp
      modify
        ( \gs ->
            gs
              { gsVars =
                  M.insert
                    n
                    Var {varPos = rsp, varSize = sz, varType = t}
                    (gsVars gs)
              }
        )
    genStatement (IWhileStatement e b) = do
      msz <- genExpr e
      when (isJust msz) (error "expected boolean for if")
      breakn <- gets gsBreakn
      loopn <- gets gsLoopn
      lift $
        tell
          ( ".L"
              ++ show loopn
              ++ ":\ncmp rax, 0\nje .B"
              ++ show breakn
              ++ "\n"
          )
      mapM_ genStatement (iblockStatements b)
      lift $ tell (".B" ++ show breakn ++ ":\n")
      modify (\gs -> gs {gsBreakn = breakn + 1, gsLoopn = loopn + 1})

    genExpr :: InferredExpr -> CodeGen (Maybe Int)
    genExpr (IVarExpr v) = do
      vars <- gets gsVars
      case M.lookup v vars of
        Just var -> case varSize var of
          8 -> do
            lift $ tell ("mov rax, [rbp - " ++ show (varPos var) ++ "]\n")
            return Nothing
          sz -> do
            modify (\gs -> gs {gsRsp = gsRsp gs + sz})
            rsp <- gets gsRsp
            lift $ tell ("sub rsp, " ++ show sz ++ "\n")
            bitMove (varPos var) rsp sz
            return (Just sz)
        Nothing -> error ("'" ++ v ++ "' not found in this scope")
    genExpr fe@IFieldExpr {} = do
      (field, toFree) <- mkVar fe
      case varSize field of
        8 -> do
          lift $ tell ("mov rax, [rbp - " ++ show (varPos field) ++ "]\n")
        _ -> todo "big field copy"
      case toFree of
        Just sz -> do
          modify (\gs -> gs {gsRsp = gsRsp gs - sz})
          lift $ tell ("add rsp, " ++ show sz ++ "\n")
        Nothing -> pure ()
      return $ case varSize field of
        8 -> Nothing
        sz -> Just sz
    genExpr (IBinExpr a bin b) = do
      msz <- genExpr a
      case msz of
        Just _ -> todo "overloaded binaries"
        Nothing -> lift $ tell "sub rsp, 8\nmov [rsp], rax\n"
      msz2 <- genExpr b
      case msz2 of
        Just _ -> todo "overloaded binaries"
        Nothing -> lift $ tell "mov rdx, rax\nmov rax, [rsp]\nadd rsp, 8\n"
      case bin of
        Equal -> do
          lift $ tell "cmp rax, rdx\nsete al\nmovzx rax, al\n"
          return Nothing
    genExpr (ILitExpr lit) = case lit of
      IntLit i -> do
        lift $ tell ("mov rax, " ++ show i ++ "\n")
        return Nothing
      StrLit s -> do
        modify (\gs -> gs {gsRsp = gsRsp gs + 16})
        consts <- gets gsConsts
        modify (\gs -> gs {gsConsts = s : consts})
        lift $
          tell
            ( "sub rsp, 16\nmov QWORD [rsp + 8], .C"
                ++ show (length consts)
                ++ "\nmov QWORD [rsp], "
                ++ show (length $ s \\ ['\\'])
                ++ "\n"
            )
        return (Just 16)
      UnitLit -> return Nothing
    genExpr (ICallExpr name args) = do
      s <- fmap sum $ forM args $ \arg -> do
        msz <- genExpr arg
        case msz of
          Nothing -> do
            modify (\gs -> gs {gsRsp = gsRsp gs + 8})
            lift $ tell "sub rsp, 8\nmov [rsp], rax\n"
            return 8
          Just sz -> return sz
      modify (\gs -> gs {gsRsp = gsRsp gs - s})
      lift $ tell ("call " ++ name ++ "\nadd rsp, " ++ show s ++ "\n")
      return Nothing
    genExpr ee@IElemExpr {} = do
      (e, toFree) <- mkVar ee
      case varSize e of
        8 -> do
          lift $ tell ("mov rax, [rbp - " ++ show (varPos e) ++ "]\n")
        _ -> todo "big elem copy"
      case toFree of
        Just sz -> do
          modify (\gs -> gs {gsRsp = gsRsp gs - sz})
          lift $ tell ("add rsp, " ++ show sz ++ "\n")
        Nothing -> pure ()
      return $ case varSize e of
        8 -> Nothing
        sz -> Just sz

    mkVar :: InferredExpr -> CodeGen (Var, Maybe Int)
    mkVar (IVarExpr v) = do
      vars <- gets gsVars
      case M.lookup v vars of
        Just var -> return (var, Nothing)
        Nothing -> error ("'" ++ v ++ "' not found in this scope")
    mkVar (IFieldExpr f e) = first (getField f) <$> mkVar e
    mkVar e = do
      msz <- genExpr e
      case msz of
        Nothing -> do
          rsp <- gets gsRsp
          modify (\gs -> gs {gsRsp = rsp + 8})
          lift $ tell "sub rsp, 8\nmov [rsp], rax\n"
          return
            ( Var
                { varPos = rsp + 8,
                  varSize = 8,
                  varType = iexprType e
                },
              Just 8
            )
        Just sz -> do
          rsp <- gets gsRsp
          return
            ( Var
                { varPos = rsp,
                  varSize = sz,
                  varType = iexprType e
                },
              Just sz
            )

    getField :: String -> Var -> Var
    getField s Var {..} = case findType varType of
      Nothing -> error $ "type '" ++ show varType ++ "' not found"
      Just (StructType Struct {..}) ->
        case lookup
          s
          ( zipWith
              (\(a, b) c -> (a, (b, c)))
              structFields
              [0 ..]
          ) of
          Nothing -> error $ "field '" ++ s ++ "' not found"
          Just (t, p) ->
            Var
              { varPos = -16 - p * 8,
                varSize = typeNameSize t,
                varType = t
              }
      Just _ -> error "trying to get field of a non-struct"

    findType :: TypeName -> Maybe Type
    findType tn = case find
      (\t -> typeName t == typeNameName tn)
      (iprogramTypes program) of
      Nothing -> error $ "type '" ++ typeNameName tn ++ "' not found"
      Just (TypeAlias _ ta) -> findType ta
      Just t -> pure t

    typeNameSize :: TypeName -> Int
    typeNameSize (Pointer _) = 8
    typeNameSize tn = case find
      (\t -> typeName t == typeNameName tn)
      (iprogramTypes program) of
      Nothing -> error $ "type '" ++ show tn ++ "' not found"
      Just t -> typeSize t

    typeSize :: Type -> Int
    typeSize Int = 8
    typeSize (StructType Struct {..}) = sum $ typeNameSize . snd <$> structFields
    typeSize (TypeAlias _ tn) = typeNameSize tn

    bitMove :: Int -> Int -> Int -> CodeGen ()
    bitMove s f sz = forM_ [0, 8 .. sz - 1] $ \i -> do
      lift $
        tell
          ( "mov rax, [rbp - "
              ++ show (s - i)
              ++ "]\nmov [rbp - "
              ++ show (f - i)
              ++ "], rax\n"
          )

iexprType :: InferredExpr -> TypeName
iexprType = todo "iexpr"

typeName :: Type -> String
typeName Int = "int"
typeName (StructType Struct {..}) = structName
typeName (TypeAlias s _) = s

typeNameName :: TypeName -> String
typeNameName (TypeName name _) = name
typeNameName (Pointer tn) = "__ptr$" ++ typeNameName tn

initGenState :: GenState
initGenState =
  GenState
    { gsLoopn = 0,
      gsBreakn = 0,
      gsRsp = 0,
      gsVars = M.empty,
      gsConsts = []
    }

die :: String -> IO a
die err = do
  putStr "error: "
  putStrLn err
  exitFailure
