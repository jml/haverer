{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.Tasty

import qualified Ring

suite :: TestTree
suite = testGroup "Test Suite" [
  Ring.suite
  ]

main :: IO ()
main = defaultMain suite
