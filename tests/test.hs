{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.Tasty

import qualified Ring
import qualified Round

suite :: TestTree
suite =
  testGroup "Test Suite" [ Ring.suite
                         , Round.suite
                         ]

main :: IO ()
main = defaultMain suite
