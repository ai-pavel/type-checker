module Main where

import Test.Hspec
import qualified Data.Map.Strict as Map
import Infer.Ast
import Infer.Types
import Infer.Unify
import Infer.Infer
import Infer.Parser

-- Helper: parse and infer
inferStr :: String -> Either String String
inferStr input = do
  expr <- parseExpr input
  case inferType defaultEnv expr of
    Left err -> Left (show err)
    Right ty -> Right ty

-- Helper: expect success
shouldInferTo :: String -> String -> Expectation
shouldInferTo input expected =
  inferStr input `shouldBe` Right expected

-- Helper: expect parse failure
shouldFailParse :: String -> Expectation
shouldFailParse input =
  case parseExpr input of
    Left _  -> return ()
    Right _ -> expectationFailure "Expected parse error"

main :: IO ()
main = hspec $ do
  describe "Literals" $ do
    it "infers Int for integer literals" $
      "42" `shouldInferTo` "Int"

    it "infers Bool for True" $
      "True" `shouldInferTo` "Bool"

    it "infers Bool for False" $
      "False" `shouldInferTo` "Bool"

    it "infers String for string literals" $
      "\"hello\"" `shouldInferTo` "String"

  describe "Lambda expressions" $ do
    it "infers identity function" $
      "\\x -> x" `shouldInferTo` "a -> a"

    it "infers const function" $
      "\\x -> \\y -> x" `shouldInferTo` "a -> b -> a"

    it "infers multi-arg lambda" $
      "\\x y -> x" `shouldInferTo` "a -> b -> a"

  describe "Function application" $ do
    it "infers application of identity" $
      "(\\x -> x) 42" `shouldInferTo` "Int"

    it "infers application of const" $
      "(\\x -> \\y -> x) True 42" `shouldInferTo` "Bool"

  describe "Let expressions" $ do
    it "infers simple let binding" $
      "let x = 42 in x" `shouldInferTo` "Int"

    it "supports let-polymorphism" $
      "let id = \\x -> x in id 42" `shouldInferTo` "Int"

    it "uses polymorphic binding at different types" $
      "let id = \\x -> x in if id True then id 1 else id 2" `shouldInferTo` "Int"

  describe "If-then-else" $ do
    it "infers if with matching branches" $
      "if True then 1 else 2" `shouldInferTo` "Int"

    it "rejects non-bool condition" $ do
      let result = inferStr "if 42 then 1 else 2"
      case result of
        Left _ -> return ()
        Right _ -> expectationFailure "Expected type error for non-bool condition"

    it "rejects mismatched branches" $ do
      let result = inferStr "if True then 1 else True"
      case result of
        Left _ -> return ()
        Right _ -> expectationFailure "Expected type error for mismatched branches"

  describe "Higher-order functions" $ do
    it "infers compose-like function" $
      "\\f -> \\g -> \\x -> f (g x)" `shouldInferTo` "(a -> b) -> (c -> a) -> c -> b"

    it "infers twice function" $
      "\\f -> \\x -> f (f x)" `shouldInferTo` "(a -> a) -> a -> a"

  describe "Unification" $ do
    it "unifies identical type constructors" $
      unify (TCon "Int") (TCon "Int") `shouldBe` Right emptySubst

    it "fails on different constructors" $ do
      let result = unify (TCon "Int") (TCon "Bool")
      case result of
        Left (UnificationFail _ _) -> return ()
        _ -> expectationFailure "Expected unification failure"

    it "binds type variable" $
      unify (TVar "a") (TCon "Int") `shouldBe` Right (Map.singleton "a" (TCon "Int"))

    it "detects infinite types" $ do
      let result = unify (TVar "a") (TFun (TVar "a") (TVar "a"))
      case result of
        Left (InfiniteType _ _) -> return ()
        _ -> expectationFailure "Expected infinite type error"

  describe "Parser" $ do
    it "parses integer literal" $
      parseExpr "42" `shouldBe` Right (ELit (LInt 42))

    it "parses lambda" $
      parseExpr "\\x -> x" `shouldBe` Right (ELam "x" (EVar "x"))

    it "parses let" $
      parseExpr "let x = 1 in x" `shouldBe` Right (ELet "x" (ELit (LInt 1)) (EVar "x"))

    it "parses if-then-else" $
      parseExpr "if True then 1 else 2" `shouldBe`
        Right (EIf (ELit (LBool True)) (ELit (LInt 1)) (ELit (LInt 2)))

    it "parses application" $
      parseExpr "f x" `shouldBe` Right (EApp (EVar "f") (EVar "x"))

    it "parses nested application left-associatively" $
      parseExpr "f x y" `shouldBe` Right (EApp (EApp (EVar "f") (EVar "x")) (EVar "y"))

    it "rejects invalid input" $
      shouldFailParse "let = in"

  describe "Case expressions" $ do
    it "infers case on literals" $
      "case 1 of | x -> x" `shouldInferTo` "Int"

    it "infers case with wildcard" $
      "case True of | _ -> 42" `shouldInferTo` "Int"
