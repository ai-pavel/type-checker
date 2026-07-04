module Main where

import System.Console.Haskeline

import Infer.Types
import Infer.Infer
import Infer.Parser

main :: IO ()
main = do
  putStrLn "Hindley-Milner Type Inference REPL"
  putStrLn "Type an expression to infer its type. Type :quit to exit."
  putStrLn ""
  runInputT defaultSettings (loop defaultEnv)

loop :: TypeEnv -> InputT IO ()
loop env = do
  minput <- getInputLine "> "
  case minput of
    Nothing      -> outputStrLn "Goodbye!"
    Just ":quit" -> outputStrLn "Goodbye!"
    Just ":q"    -> outputStrLn "Goodbye!"
    Just ""      -> loop env
    Just input   -> do
      case parseExpr input of
        Left err -> outputStrLn $ "Parse error: " ++ err
        Right expr ->
          case inferType env expr of
            Left err -> outputStrLn $ "Type error: " ++ show err
            Right ty -> outputStrLn ty
      loop env
