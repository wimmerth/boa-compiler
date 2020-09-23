-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
   truthy, operate, apply,
   eval, exec, execute)
  where

import BoaAST
import Control.Monad

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }

--basic instantiation of Monad, append the 'output' at the end of the list and
--return an error if one of the computations returns an error
instance Monad Comp where
  return a = Comp $ \_ -> (Right a,mempty)
  (>>=) m f = Comp $ \e -> case runComp m e of
    (Left err,xs) -> (Left err, xs)
    (Right v,xs) -> case runComp (f v) e of
      (Left err,ys) -> (Left err, xs `mappend` ys)
      (Right n, ys) -> (Right n, xs `mappend` ys)

-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

-- Operations of the monad

--just return an error
abort :: RunError -> Comp a
abort err = Comp $ \_ -> (Left err, mempty)

--if n could not be found, return error
look :: VName -> Comp Value
look n = Comp $ \e -> case lookup n e of
  Nothing -> (Left (EBadVar n), mempty)
  (Just v) -> (Right v, mempty)

--prepend binding to environment because 'lookup' returns first occurence in list
withBinding :: VName -> Value -> Comp a -> Comp a
withBinding n v m = Comp $ \e -> runComp m ((n,v):e)

--only add string to string array
output :: String -> Comp ()
output s = Comp $ \_ -> (Right (),[s])

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy NoneVal = False
truthy FalseVal = False
truthy (IntVal 0) = False
truthy (StringVal "") = False
truthy (ListVal []) = False
truthy _ = True

--return error if the wrong types are used for any operation or division by zero
--is attempted.
operate :: Op -> Value -> Value -> Either String Value
operate Plus (IntVal a) (IntVal b) = Right (IntVal (a+b))
operate Plus _ _ = Left "Plus only works with values of type IntVal"
operate Minus (IntVal a) (IntVal b) = Right (IntVal (a-b))
operate Minus _ _ = Left "Minus only works with values of type IntVal"
operate Times (IntVal a) (IntVal b) = Right (IntVal (a*b))
operate Times _ _ = Left "Times only works with values of type IntVal"
operate Div (IntVal a) (IntVal b) = if b == 0
  then Left "Division by zero"
  else Right (IntVal (a `div` b))
operate Div _ _ = Left "Div only works with values of type IntVal"
operate Mod (IntVal a) (IntVal b) = if b == 0
  then Left "Division by zero"
  else Right (IntVal (a `mod` b))
operate Mod _ _ = Left "Mod only works with values of type IntVal"
operate Eq a b = if a == b
  then Right TrueVal
  else Right FalseVal
operate Less (IntVal a) (IntVal b) = if a < b
  then Right TrueVal
  else Right FalseVal
operate Less _ _ = Left "Less only works with values of type IntVal"
operate Greater (IntVal a) (IntVal b) = if a > b
  then Right TrueVal
  else Right FalseVal
operate Greater _ _ = Left "Greater only works with values of type IntVal"
operate In v (ListVal xs) = if v `elem` xs
  then Right TrueVal
  else Right FalseVal
operate In _ _ = Left "The second operand of In has to ba a ListVal"

--return error if range gets 0 or >3 Values in the array, one of the parameters
--of range is not an IntVal or an unknown function is called.
apply :: FName -> [Value] -> Comp Value
apply f xs
  |f == "range" && (length xs == 0) = abort (EBadArg "range needs at least one parameter")
  |f == "range" && (length xs > 3) = abort (EBadArg "range takes at most three parameters")
  |f == "range" && (length xs == 1) = case xs !! 0 of
    (IntVal i) -> return (ListVal [IntVal n | n <- [0..i-1]])
    _ -> abort (EBadArg "range only takes values of type IntVal as parameter")
  |f == "range" && (length xs == 2) = case xs !! 0 of
    (IntVal a) -> case xs !! 1 of
      (IntVal b) -> return (ListVal [IntVal n | n <- [a..b-1]])
      _ -> abort (EBadArg "range only takes values of type IntVal as parameter")
    _ -> abort (EBadArg "range only takes values of type IntVal as parameter")
  |f == "range" && (length xs == 3) = case xs !! 0 of
    (IntVal a) -> case xs !! 1 of
      (IntVal b) -> case xs !! 2 of
        (IntVal c) -> if c == 0
          then abort (EBadArg "step must not be 0")
          else if c > 0
            then return (ListVal [IntVal n | n <- [a..b-1],(n-a)`mod`c == 0])
            else return $ ListVal $ reverse [IntVal n | n <- [b+1..a],(a-n) `mod` (abs c) == 0]
        _ -> abort (EBadArg "range only takes values of type IntVal as parameter")
      _ -> abort (EBadArg "range only takes values of type IntVal as parameter")
    _ -> abort (EBadArg "range only takes values of type IntVal as parameter")
  --use auxiliary function to pretty print the single values and cut the last ' '
  |f == "print" = let str = concatMap (\x -> toStr x ++ " ") xs in output (take (length str - 1) str) >> (return NoneVal)
  |otherwise = abort (EBadFun f)
    where
      --auxiliary function to pretty print values
      toStr :: Value -> String
      toStr NoneVal = "None"
      toStr TrueVal = "True"
      toStr FalseVal = "False"
      toStr (IntVal i) = show i
      toStr (StringVal s) = s
      toStr (ListVal xs) = let str = concatMap (\x-> toStr x ++ ", ") xs in "[" ++ (take (length str - 2) str) ++ "]"


-- Main functions of interpreter
eval :: Exp -> Comp Value
eval (Const v) = return v
eval (Var v) = look v
eval (Oper op a b) = do
  aR <- eval a
  bR <- eval b
  case operate op aR bR of
    (Left s) -> abort (EBadArg s)
    (Right v) -> return v
eval (Not e) = do
  eR <- eval e
  if truthy eR
    then return FalseVal
    else return TrueVal
eval (Call f xs) = do
  ys <- mapM eval xs
  apply f ys
eval (List xs) = do
  ys <- mapM eval xs
  return (ListVal ys)
eval (Compr e xs) = do
  ys <- evalCompr e xs
  return $ ListVal ys

  --auxiliary function that evaluates all expressions of the List comprehension
  --uses Comp [Value] to be able to return "nothing" when CCIf fails, uses
  --Comp [Value] to be able to do the recursive call with withBinding
  where
    evalCompr :: Exp -> [CClause] -> Comp [Value]
    evalCompr e [] = sequence [eval e]
    evalCompr e ((CCIf f):xs) = do
      x <- eval f
      if truthy x
        then evalCompr e xs
        else return []
    evalCompr e ((CCFor v f):xs) = do
      x <- eval f
      case x of
        (ListVal ys) -> do
          zs <- sequence [withBinding v y (evalCompr e xs)| y <- ys]
          return $ concat zs
        _ -> sequence [abort $ EBadArg "CCFor needs a ListVal as parameter"]

--executes program but returns Comp
exec :: Program -> Comp ()
exec [] = return ()
exec ((SExp e):xs) = (eval e) >> (exec xs)
exec ((SDef v e):xs) = do
  x <- eval e
  withBinding v x (exec xs)

--executes program and explicitly return output list and RunError if there was one
execute :: Program -> ([String], Maybe RunError)
execute p = let (a,b) = runComp (exec p) [] in (b,case a of
  Left e -> Just e
  Right _ -> Nothing)
