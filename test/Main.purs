module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.Maybe
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

import Dict
import Translator (t, tOrGuess)
import Data.String

structure :: Dict String
structure =
  Branch "a" [
    (Branch "b" [
      Leaf "c" "a b c"
    ])
    , Leaf "d" "efg hij"
  ]

main :: Effect Unit
main = runTest do
  suite "Tree Fun Time" do
     test "Can Translate" do
       Assert.equal (t "a.b" $ Leaf "a.b" "b") (Just "b")
       Assert.equal (t "a" $ Leaf "a" "b") (Just "b")
       Assert.equal (t ".a" $ Leaf "a" "b") Nothing
       Assert.equal (t ".a" $ Branch "" [Leaf "a" "t"]) Nothing
       Assert.equal (t "" $ Leaf "a" "b") Nothing
       Assert.equal (t "a." $ Leaf "a" "b") Nothing
       Assert.equal (t "b" $ Leaf "a" "b") Nothing
       Assert.equal (t "a" $ Branch "a" []) Nothing
       Assert.equal (t "b" $ Branch "a" []) Nothing
       Assert.equal (t "a" $ Branch "a" [Leaf "b" "c"]) Nothing
       Assert.equal (t "a.b" $ Branch "a" [Leaf "b" "c"]) (Just "c")
     test "Can Guess" do
       Assert.equal (tOrGuess "a" $ Leaf "a" "b") "b"
       Assert.equal (tOrGuess "" $ Leaf "a" "b") ""
       Assert.equal (tOrGuess "abc" $ Leaf "b" "b") "Abc"
       Assert.equal (tOrGuess "ab_cd" $ Leaf "b" "b") "Ab Cd"
  suite "Tree mapping" do
     test "Functor Laws" do
        Assert.equal (map identity structure) structure
        Assert.equal (split (Pattern " ") <$> toUpper <$> structure) (split (Pattern " ") <<< toUpper <$> structure)
  suite "Tree Eq" do
     test "Eq" do
        Assert.equal (Leaf "a" "b") (Leaf "a" "b")
        Assert.equal structure structure
        Assert.assertFalse "Should not have matched" (Leaf "a" "b" == Leaf "c" "d")
        Assert.assertFalse "Should not have matched" (Leaf "a" "b" == Branch "c" [Leaf "e" "d"])
        Assert.assertFalse "Should not have matched" (Branch "c" [Leaf "a" "b"] == Branch "c" [Leaf "e" "d"])

