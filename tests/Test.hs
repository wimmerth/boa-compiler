import BoaAST
import BoaParser
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Debug.Trace

----------generating more or less meaning full arbitrary programs----------------

asciiLetter = elements (['a'..'z']++['A'..'Z'])

--arbitrary operation
instance Arbitrary Op where
  arbitrary = oneof $ map return [Plus, Minus, Times, Div, Mod, Eq, Less, Greater, In]

--arbitrary value
instance Arbitrary Value where
  arbitrary = oneof [return NoneVal, return TrueVal, return FalseVal, randIntVal, randStringVal]
    where
      randIntVal = do
        i <- arbitrary
        return $ IntVal i
      randStringVal = do
        n <- choose (1,4)
        s <- vectorOf n asciiLetter
        return $ StringVal s
        --has been removed, since parser always [] as expression and not as value
      {-randListVal i = do
        n <- choose (1,4)
        xs <- vectorOf n $ arb $ i`div`2
        return $ ListVal xs-}

--arbitrary expression
arbitraryExp :: [String] -> Gen Exp
arbitraryExp xs = sized (\x -> arb xs x)
  where
    arb :: [VName] -> Int -> (Gen Exp)
    arb xs 0 = oneof [randConst, randVar xs]
    arb xs n = oneof [randConst, randVar xs, randOper xs n, randNot xs n, randList xs n, randCall xs n ,randCompr xs n]
    randConst = do
      v <- arbitrary
      return $ Const v
    randVar xs = if length xs == 0
      then randConst
      else do
        v <- elements xs
        return $ Var v
    randOper xs n = do
      o <- arbitrary
      e <- arb xs (n`div`2)
      f <- arb xs (n`div`2)
      return $ Oper o e f
    randNot xs n = do
      e <- arb xs (n`div`2)
      return $ Not e
    randList xs n = do
      i <- choose (0,3)
      ys <- vectorOf i $ arb xs $ n `div` 2
      return $ List ys
    randCall xs n = oneof [randCallPrint xs n, randCallRange xs n, randCallDif]
    randCallDif = do
      s <- asciiLetter
      return $ Call [s] []
    randCallPrint xs n = do
      i <- choose (0,4)
      ys <- vectorOf i $ arb xs $ n `div` 2
      return $ Call "print" ys
    randCallRange _ _ = do
      i <- choose (0,4)
      ys <- vectorOf i arbitInt
      return $ Call "range" ys
        where
          arbitInt = do
            j <- arbitrary :: Gen Int
            return $ Const $ IntVal j
    randCompr xs n = do
      (CC x e) <- randFor xs $ n`div`2
      i <- choose (0,3)
      ys <- randCC (x:xs) n i
      f <- arb xs (n`div`2)
      return $ Compr f (e++ys)
        where
          randFor :: [String] -> Int -> Gen CC
          randFor xs n = do
            x <- asciiLetter
            e <- randList' xs n
            return $ CC [x] $ [CCFor [x] e]
          randIf :: [String] -> Int -> Gen CC
          randIf xs n = do
            e <- arb xs n
            return $ CC "" $ [CCIf e]
          randList' xs n = oneof [randList xs n, randCallRange xs n]
          randCC :: [String] -> Int -> Int -> Gen [CClause]
          randCC _ _ 0 = return []
          randCC xs n i = do
            (CC x e) <- oneof [randFor xs (n `div` 2), randIf xs (n `div` 2)]
            if null x
              then do
                fs <- randCC xs n (i-1)
                return $ e++fs
              else do
                fs <- randCC (x:xs) n (i-1)
                return $ e++fs

--custom datatypes in order to create arbitrary programs using the same variable set
data CC = CC String [CClause]
data S = S String Stmt
--since Program is just a type, we have to create a newtype in order to instantiate Arbitrary
newtype P = P Program
  deriving (Eq,Show)

--arbitrary statement
arbitraryS :: [String] -> Gen S
arbitraryS xs = oneof [arbSDef xs, arbSExp xs]
  where
    arbSDef xs = do
      x <- asciiLetter
      e <- arbitraryExp xs
      return $ S [x] $ SDef [x] e
    arbSExp xs = do
      e <- arbitraryExp xs
      return $ S "" $ SExp e

--arbitrary program
instance Arbitrary P where
  arbitrary = sized (\x -> if x == 0 then arb [] 5 else arb [] x)
    where
      arb :: [String] -> Int -> Gen P
      arb _ 0 = return $ P []
      arb xs n = do
        (S x y) <- arbitraryS xs
        if null x
          then do
            (P ys) <- arb xs (n-1)
            return $ P $ y:ys
          else do
            (P ys) <- arb (x:xs) (n-1)
            return $ P $ y:ys

--------------------print a program as input to parser--------------------------
printProgram :: Program -> String
printProgram [] = ""
printProgram [x] = printStatement x ++ "\n"
printProgram (x:xs) = printStatement x ++ ";\n" ++ printProgram xs

printStatement :: Stmt -> String
printStatement (SDef v e) = v ++ " = (" ++ printExpression e ++ ")"
printStatement (SExp e) = printExpression e

--print expression: heavy bracketing in order to remove ambiguity
printExpression :: Exp -> String
printExpression (Const v) = printValue v
  where
    printValue NoneVal = "None"
    printValue TrueVal = "True"
    printValue FalseVal = "False"
    printValue (IntVal i) = show i
    printValue (StringVal s) = "'" ++ s ++ "'"
    printValue (ListVal vs) = "[" ++ take (length list - 2) list ++ "]"
      where list = concat ["(" ++ printValue v ++ "), " | v <- vs]
printExpression (Var s) = s
printExpression (Oper o e f) = "((" ++ printExpression e ++ ") " ++ printOperation o ++ " (" ++ printExpression f ++ "))"
  where
    printOperation Plus = "+"
    printOperation Minus = "-"
    printOperation Times = "*"
    printOperation Div = "//"
    printOperation Mod = "%"
    printOperation Eq = "=="
    printOperation Less = "<"
    printOperation Greater = ">"
    printOperation In = "in"
printExpression (Not e) = "(not (" ++ printExpression e ++ "))"
printExpression (Call s es) = "(" ++ s ++ "(" ++ take (length list - 2) list ++ "))"
  where list = concat [printExpression e ++ ", " | e <- es]
printExpression (List es) = "[" ++ take (length list - 2) list ++ "]"
  where list = concat ["(" ++ printExpression e ++ "), " | e <- es]
printExpression (Compr e cs) = "[(" ++ printExpression e ++ ") " ++ take (length list - 1) list ++ "]"
  where
    list = concat [printCClause c ++ " " | c <- cs]
    printCClause (CCIf e) = "if (" ++ printExpression e ++ ")"
    printCClause (CCFor s e) = "for " ++ s ++ " in (" ++ printExpression e ++ ")"

-------------------------actual tests-------------------------------------------

--test parsing of arbitrary programs
propEquality :: P -> Bool
propEquality (P ps) = case parseString (printProgram ps) of
  (Left _) -> trace "fehler" False
  (Right p) -> p == ps


main :: IO ()
main = defaultMain $ localOption (mkTimeout 10000000) tests

tests = testGroup "Tests"
  [
  testProperty "Parse Arbitrary programs and check for equality (may take a second or two)" propEquality,
  testGroup "Specific unit tests" [
    testGroup "Test operator associativity/precedence" [
      testCase "associativity of add" $ parseString "1+2+3" @?= (Right [SExp (Oper Plus (Oper Plus (Const (IntVal 1)) (Const (IntVal 2))) (Const (IntVal 3)))]),
      testCase "associativity of mul" $ parseString "1*2*3" @?= (Right [SExp (Oper Times (Oper Times (Const (IntVal 1)) (Const(IntVal 2))) (Const (IntVal 3)))]),
      testCase "associativity of div/mod" $ parseString "1//2%3" @?= (Right [SExp (Oper Mod (Oper Div (Const (IntVal 1)) (Const(IntVal 2))) (Const (IntVal 3)))]),
      testCase "associativity of minus" $ parseString "1-2-3" @?= (Right [SExp (Oper Minus (Oper Minus (Const (IntVal 1)) (Const(IntVal 2))) (Const (IntVal 3)))]),
      testCase "precedence test" $ parseString "not 1+2*3-(not 4//5+6)" @?= (Right[SExp (Not(Oper Minus (Oper Plus (Const (IntVal 1)) (Oper Times (Const (IntVal 2)) (Const (IntVal 3)))) (Not (Oper Plus (Oper Div (Const (IntVal 4)) (Const (IntVal 5))) (Const (IntVal 6))))))]),
      testCase "no associativity of <,>,==" $ parseString "1 < 2 > 3 == 4" @?= (Left "cannot parse")
      ],
    testGroup "Tests of skip/comments" [
      testCase "missing whitespace between keywords" $ parseString "a notin b" @?= (Left "cannot parse"),
      testCase "no whitespace, but bracket" $ parseString "not(False)" @?= (Right [SExp (Not (Const FalseVal))]),
      testCase "skipping comments" $ parseString "True#comment\n#anotherone\t\n;False" @?= (Right [SExp (Const TrueVal),SExp (Const FalseVal)]),
      testCase "skipping comments at eof" $ parseString "True#commentateof" @?= (Right [SExp (Const TrueVal)]),
      testCase "empty comment" $ parseString "True;#\nFalse" @?= (Right [SExp (Const TrueVal),SExp (Const FalseVal)]),
      testCase "skipping newlines" $ parseString "True\n\n;\nFalse" @?= (Right [SExp (Const TrueVal),SExp (Const FalseVal)]),
      testCase "skipping tabs" $ parseString "tab\t\t\t;False" @?= (Right [SExp (Var "tab"),SExp (Const FalseVal)]),
      testCase "comment as one whitespace" $ parseString "not#comment\ncool" @?= (Right [SExp (Not (Var "cool"))])
      ],
    testGroup "Tests of pIdent" [
      testCase "keyword as identifier" $ parseString "in = out" @?= (Left "cannot parse"),
      testCase "keyword in identifier" $ parseString "inside = outside" @?= (Right [SDef "inside" (Var "outside")]),
      testCase "starting with _" $ parseString "_underscore = UP" @?= (Right [SDef "_underscore" (Var "UP")]),
      testCase "starting with number" $ parseString "112 = alarm" @?= (Left "cannot parse"),
      testCase "numbers, underscore and letters" $ parseString "_cr7 < Messi10_" @?= (Right [SExp (Oper Less (Var "_cr7") (Var "Messi10_"))])
      ],
    testGroup "Tests of pNumConst" [
      testCase "minus zero '-0'" $ parseString "-0" @?= (Right [SExp (Const (IntVal 0))]),
      testCase "plus in front of number '+0'" $ parseString "+0" @?= (Left "cannot parse"),
      testCase "wrong number format '1.0'" $ parseString "1.0" @?= (Left "cannot parse"),
      testCase "space between minus and number '- 911'" $ parseString "- 911" @?= (Left "cannot parse"),
      testCase "leading zeros '007'" $ parseString "007" @?= (Left "cannot parse")
      ],
    testGroup "Tests of pStringConst" [
      testCase "'basic string'" $ parseString "'basic string'" @?= (Right [SExp (Const (StringVal "basic string"))]),
      testCase "'a\\nb'" $ parseString "'a\\\nb'" @?= (Right [SExp (Const (StringVal "ab"))]),
      testCase "'\\\\'" $ parseString "'\\\\'" @?= (Right [SExp (Const (StringVal "\\"))]),
      testCase "'a#bc'" $ parseString "'a#bc'" @?= (Right [SExp (Const (StringVal "a#bc"))]),
      testCase "'\\''" $ parseString "'\\''" @?= (Right [SExp (Const (StringVal "'"))]),
      testCase "'\\x'" $ parseString "'\\x'" @?= (Left "cannot parse")
      ]
    ]
  ]
