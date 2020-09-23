-- This module defines a simple command line interface for the Boa interpreter.
module Main (main) where

import BoaAST
import BoaInterp (execute)
import BoaParser (parseString)

import System.Exit (die)
import System.Environment (getArgs)

run :: Program -> IO ()
run p =
  do let (out, res) = execute p
     mapM_ putStrLn out
     case res of
       Nothing -> return ()
       Just e -> putStrLn ("*** Runtime error: " ++ show e)

main :: IO ()
main = do args <- getArgs
          case args of
            ["-i", file] -> do
              s <- readFile file
              run $ read s
            ["-p", file] -> do
              s <- readFile file
              case parseString s of
                Left e -> putStrLn $ "*** Parse error: " ++ show e
                Right p -> putStrLn $ show p
            [file] -> do
              s <- readFile file
              case parseString s of
                Left e -> putStrLn $ "*** Parse error: " ++ show e
                Right p -> run p
            _ ->
              die "Usage:\n\
                    \  boa -i PROGRAM.ast    (interpret only)\n\
                    \  boa -p PROGRAM.boa    (parse only)\n\
                    \  boa PROGRAM.boa       (parse & interpret)"
