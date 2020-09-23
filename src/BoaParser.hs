{-
Stmts     ::= Stmt Stmt'
Stmt'     ::= ";" Stmts | €
Stmt      ::= ident "=" Exp
            | Exp
Exp       ::= "not" Exp
            | ExpOrd
ExpOrd    ::= ExpAdd ExpOrd'
ExpOrd'   ::= OpOrd ExpAdd | €
OpOrd     ::= "==" | "!=" | "<" | "<=" | ">" | ">=" | "in" | "not in"
ExpAdd    ::= ExpMul ExpAdd'
ExpAdd'   ::= OpAdd ExpMul ExpAdd' | €
OpAdd     ::= "+" | "-"
ExpMul    ::= ExpTerm ExpMul'
ExpMul'   ::= OpMul ExpTerm ExpMul' | €
OpMul     ::= "*" | "//" | "%"
ExpTerm   ::= numConst
            | stringCst
            | "None"
            | "True"
            | "False"
            | ident ExpI
            | "(" Exp ")"
            | "[" ExpB "]"
ExpI      ::= "(" Expz ")" | €
Expz      ::= Exp Expzs | €
Expzs     ::= "," Exp Expzs | €
ExpB      ::= Exp ExpB' | €
ExpB'     ::= Exps | ForClause Clausez
ExpI      ::= "(" Expz ")" | €
Expz      ::= Exp Expzs | €
Expzs     ::= "," Exp Expzs | €
ExpB      ::= Exp ExpB' | €
ExpB'     :: Exps | ForClause Clausez
Exps      ::= "," Exp Exps | €
ForClause ::= "for" ident "in" Exp
IfClause  ::= "if" Exp
Clausez   ::= ForClause Clausez | IfClause Clausez | €

ident     ::= (a|_)(a|_|1)* [/= "None","True","False","for","if","in"]
numConst  ::= (-|€)(0|(1..9)(0..9)*)
stringCst ::= '(ascii|\\|\'|\\n)'
-}

module BoaParser (ParseError, parseString) where

import BoaAST
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char
import Control.Monad

type Parser a = ReadP a
type ParseError = String

parseString :: String -> Either ParseError Program
parseString s = case readP_to_S (do skip; a <- pStmts; eof; return a) s of
  [] -> Left "cannot parse"
  [(a,_)] -> Right a
  _ -> error "oops, my grammar is ambiguos"

--works like skipSpaces but also skips comments
skip :: Parser ()
skip = do
  skipSpaces;
  s <- look
  when (not (null s) && head s == '#') $ do
    manyTill get (satisfy (== '\n') <|> do eof; return 'a')
    skip

--like skip, but there must be at least one whitespace
skip1 :: Parser ()
skip1 = do
  s <- look
  if not (null s) && head s == '#'
    then skip
    else do
      munch1 isSpace
      skip

--like skip, but there must be at least one whitespace or the next expression starts with a bracket
skipOne :: Parser ()
skipOne = do
  s <- look
  if not (null s) && (head s == '(' || head s == '[')
    then return ()
    else if head s == '#'
      then skip
      else do
        munch1 isSpace
        skip

--parses an identifier
--does not skip at the end, in order to make the use of skip1 possible where needed
pIdent :: Parser String
pIdent = do
  s <- look
  if any (\str -> take (length str) s == str && (null (drop (length str) s) || not(stringChar (s !! max 0 (length str))))) ["None","True","False","for","if","in","not"]
    then pfail
    else do
      c <- satisfy (\c -> isAlpha c || c=='_')
      cs <- munch stringChar
      return (c:cs)
  where
    stringChar :: Char -> Bool
    stringChar c = isAlphaNum c || c=='_'

--parses a numeric constant
pNumConst :: Parser Exp
pNumConst = do char '-'; pNumConst' "-"
  <|> pNumConst' ""

pNumConst' :: String -> Parser Exp
pNumConst' b = do char '0'; return (Const (IntVal 0))
  <|> do
    c <- satisfy (\c -> c `elem` ['1','2','3','4','5','6','7','8','9'])
    cs <- munch isNumber
    skip;
    return (Const (IntVal (read (b ++ c:cs))))

--parses a string constant
pStringConst :: Parser Exp
pStringConst = do
  char '\''
  str <- pStr ""
  skip;
  return (Const (StringVal str))

pStr :: String -> Parser String
pStr s = do char '\\'; char 'n'; pStr $ s ++ "\n"
  <|> do char '\\'; char '\''; pStr $ s ++ "'"
  <|> do char '\\'; char '\\'; pStr $ s ++ "\\"
  <|> do char '\\'; char '\n'; pStr s
  <|> do c <- satisfy (\c -> c /= '\\' && c /= '\'' && isPrint c); pStr $ s ++ [c]
  <|> do char '\''; return s

--Stmts     ::= Stmt Stmt'
pStmts :: Parser [Stmt]
pStmts = do s <- pStmt; ss <- pStmt'; skip; return $ s:ss

--Stmt'     ::= ";" Stmts | €
pStmt' :: Parser [Stmt]
pStmt' = do char ';'; skip; pStmts
  <|> return []

--Stmt      ::= ident "=" Exp
--            | Exp
pStmt :: Parser Stmt
pStmt = do i <- pIdent; skip; char '='; skip; e<-pExp; return $ SDef i e
  <|> do e <- pExp; return $ SExp e

--Exp       ::= "not" Exp
--            | ExpOrd
pExp :: Parser Exp
pExp = do string "not"; skipOne; e <- pExp; return (Not e)
  <|> pExpOrd

--ExpOrd    ::= ExpAdd ExpOrd'
pExpOrd :: Parser Exp
pExpOrd = do
  e <- pExpAdd
  pExpOrd' e

--ExpOrd'   ::= OpOrd ExpAdd | €
--OpOrd     ::= "==" | "!=" | "<" | "<=" | ">" | ">=" | "in" | "not in"
pExpOrd' :: Exp -> Parser Exp
pExpOrd' e = do f <- helper "=="; return (Oper Eq e f)
  <|> do f <- helper "!="; return (Not (Oper Eq e f))
  <|> do f <- helper "<"; return (Oper Less e f)
  <|> do f <- helper "<="; return (Not (Oper Greater e f))
  <|> do f <- helper ">"; return (Oper Greater e f)
  <|> do f <- helper ">="; return (Not (Oper Less e f))
  <|> do f <- helperOne "in"; return (Oper In e f)
  <|> do string "not"; skip1; f <- helperOne "in"; return (Not (Oper In e f))
  <|> return e
  where
    helper s = do string s; skip; pExpAdd
    helperOne s = do string s; skipOne; pExpAdd

--ExpAdd    ::= ExpMul ExpAdd'
pExpAdd :: Parser Exp
pExpAdd = do e <- pExpMul; pExpAdd' e

--ExpAdd'   ::= OpAdd ExpMul ExpAdd' | €
--OpAdd     ::= "+" | "-"
pExpAdd' :: Exp -> Parser Exp
pExpAdd' e = do char '+'; skip; f <- pExpMul; pExpAdd' (Oper Plus e f)
  <|> do char '-'; skip; f <- pExpMul; pExpAdd' (Oper Minus e f)
  <|> return e

--ExpMul    ::= ExpTerm ExpMul'
--ExpMul'   ::= OpMul ExpTerm ExpMul' | €
--OpMul     ::= "*" | "//" | "%"
pExpMul :: Parser Exp
pExpMul = do e <- pExpTerm; pExpMul' e

pExpMul' :: Exp -> Parser Exp
pExpMul' e = do char '*'; skip; f <- pExpTerm; pExpMul' (Oper Times e f)
  <|> do char '%'; skip; f <- pExpTerm; pExpMul' (Oper Mod e f)
  <|> do string "//"; skip; f <- pExpTerm; pExpMul' (Oper Div e f)
  <|> return e

--ExpTerm   ::= numConst
--            | stringCst
--            | "None"
--            | "True"
--            | "False"
--            | ident ExpI
--            | "(" Exp ")"
--            | "[" ExpB "]"
pExpTerm :: Parser Exp
pExpTerm = pNumConst
  <|> pStringConst
  <|> do string "None"; skip; return (Const NoneVal)
  <|> do string "True"; skip; return (Const TrueVal)
  <|> do string "False"; skip; return (Const FalseVal) --skip1
  <|> do i <- pIdent; skip; pExpI i
  <|> do char '('; skip; e <- pExp; char ')'; skip; return e
  <|> do char '['; skip; e <- pExpB; char ']'; skip; return e

--ExpI      ::= "(" Expz ")" | €
pExpI :: String -> Parser Exp
pExpI s = do char '('; skip; e <- pExpz s; char ')'; skip; return e
  <|> return (Var s)

--Expz      ::= Exp Expzs | €
pExpz :: String -> Parser Exp
pExpz s = do e <- pExp; pExpzs s [e]
  <|> return (Call s [])

--Expzs     ::= "," Exp Expzs | €
pExpzs :: String -> [Exp] -> Parser Exp
pExpzs s es = do char ','; skip; e <- pExp; pExpzs s (es++[e])
  <|> return (Call s es)

--ExpB      ::= Exp ExpB' | €
pExpB :: Parser Exp
pExpB = do e <- pExp; pExpB' e
  <|> return (List [])

--ExpB'     ::= Exps | ForClause Clausez
pExpB' :: Exp -> Parser Exp
pExpB' e = pExps [e]
  <|> do f <- pForClause; pClausez e [f]

--Exps      ::= "," Exp Exps | €
pExps :: [Exp] -> Parser Exp
pExps es = do char ','; skip; e <- pExp; pExps (es ++ [e])
  <|> return (List es)

--ForClause ::= "for" ident "in" Exp
pForClause :: Parser CClause
pForClause = do
  string "for"
  skip1
  i <- pIdent
  skipOne
  string "in"
  skipOne
  e <- pExp
  skip;
  return (CCFor i e)

--IfClause  ::= "if" Exp
pIfClause :: Parser CClause
pIfClause = do
  string "if"
  skipOne
  e <- pExp
  skip;
  return (CCIf e)

--Clausez   ::= ForClause Clausez | IfClause Clausez | €
pClausez :: Exp -> [CClause] -> Parser Exp
pClausez e cs = do f <- pForClause; pClausez e (cs++[f])
  <|> do i <- pIfClause; pClausez e (cs++[i])
  <|> return (Compr e cs)
