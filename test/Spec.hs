-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/haskell-data-maybe-preserve/master/LICENSE

{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import Test.Hspec (hspec, describe, it, shouldBe)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, tryTakeMVar)

import Data.Maybe.Preserve
     ( preserve,  preserve'
     , preserveF, preserveF', lazyPreserveF'
     , preserveM, preserveM'
     )


main ∷ IO ()
main = hspec $ do

  describe "preserve" $ do

    it "`preserve` that returns Just if value passes a predicate" $ do
      preserve (== 10) 10 `shouldBe` (Just 10 ∷ Maybe Int)
      preserve (== 10) 20 `shouldBe` (Nothing ∷ Maybe Int)
      preserve (== 20) 20 `shouldBe` (Just 20 ∷ Maybe Int)

    it "`preserve'` alternative version of `preserve`\
       \ (just Bool instead of predicate)" $ do
      preserve' True  10 `shouldBe` (Just 10 ∷ Maybe Int)
      preserve' False 20 `shouldBe` (Nothing ∷ Maybe Int)
      preserve' True  20 `shouldBe` (Just 20 ∷ Maybe Int)

  describe "preserveF" $ do

    it "`preserveF` monadic version of `preserveF`" $ do
      preserveF (== 10) (return 10 ∷ IO Int) >>= (`shouldBe` Just 10)
      preserveF (== 10) (return 20 ∷ IO Int) >>= (`shouldBe` Nothing)
      preserveF (== 20) (return 20 ∷ IO Int) >>= (`shouldBe` Just 20)

    it "`preserveF'` alternative version of `preserveF`\
       \ (just Bool instead of predicate)" $ do
      preserveF' True  (return 10 ∷ IO Int) >>= (`shouldBe` Just 10)
      preserveF' False (return 20 ∷ IO Int) >>= (`shouldBe` Nothing)
      preserveF' True  (return 20 ∷ IO Int) >>= (`shouldBe` Just 20)

    it "`lazyPreserveF'` lazy version of `preserveF'`" $ do
      lazyPreserveF' True  (return 10 ∷ IO Int) >>= (`shouldBe` Just 10)
      lazyPreserveF' False (return 20 ∷ IO Int) >>= (`shouldBe` Nothing)
      lazyPreserveF' True  (return 20 ∷ IO Int) >>= (`shouldBe` Just 20)

    it "`preserveF` executes monad even if predicate is constantly falsy" $ do
      mvar ← newEmptyMVar
      let m = putMVar mvar () >> return (10 ∷ Int)
      preserveF (const True) m >>= (`shouldBe` Just 10)
      tryTakeMVar mvar >>= (`shouldBe` Just ())
      tryTakeMVar mvar >>= (`shouldBe` Nothing)
      preserveF (const False) m >>= (`shouldBe` Nothing)
      tryTakeMVar mvar >>= (`shouldBe` Just ())
      tryTakeMVar mvar >>= (`shouldBe` Nothing)

    it "`preserveF'` executes monad even if condition is False" $ do
      mvar ← newEmptyMVar
      let m = putMVar mvar () >> return (10 ∷ Int)
      preserveF' True m >>= (`shouldBe` Just 10)
      tryTakeMVar mvar >>= (`shouldBe` Just ())
      tryTakeMVar mvar >>= (`shouldBe` Nothing)
      preserveF' False m >>= (`shouldBe` Nothing)
      tryTakeMVar mvar >>= (`shouldBe` Just ()) -- Monad was executed

    it "`lazyPreserveF'` executes monad only if condition is True" $ do
      mvar ← newEmptyMVar
      let m = putMVar mvar () >> return (10 ∷ Int)
      lazyPreserveF' True m >>= (`shouldBe` Just 10)
      tryTakeMVar mvar >>= (`shouldBe` Just ())
      tryTakeMVar mvar >>= (`shouldBe` Nothing)
      lazyPreserveF' False m >>= (`shouldBe` Nothing)
      tryTakeMVar mvar >>= (`shouldBe` Nothing) -- Monad was NOT executed

  describe "preserveM" $ do

    it "`preserveM` Maybe to Maybe version of `preserve`" $ do
      preserveM (== 10) (Just 10 ∷ Maybe Int) `shouldBe` Just 10
      preserveM (== 10) (Just 20 ∷ Maybe Int) `shouldBe` Nothing
      preserveM (== 20) (Just 20 ∷ Maybe Int) `shouldBe` Just 20

    it "`preserveM'` alternative version of `preserveM`\
       \ (just Bool instead of predicate)" $ do
      preserveM' True  (Just 10 ∷ Maybe Int) `shouldBe` Just 10
      preserveM' False (Just 20 ∷ Maybe Int) `shouldBe` Nothing
      preserveM' True  (Just 20 ∷ Maybe Int) `shouldBe` Just 20
