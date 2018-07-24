module Translator (
  t
  , tOrGuess
) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), drop, take, joinWith, split, toUpper)
import Dict

tOrGuess :: String -> Dict String -> String
tOrGuess k nodes =
  case t k nodes of
    Just s -> s
    Nothing -> guess k
    where
      guess k = split (Pattern "_") k # map capitalize # joinWith " "

t :: String -> Dict String -> Maybe String
t k d = get k d >>= value
  where
    value (Branch _k _lst) = Nothing
    value (Leaf _k v) = Just v

capitalize :: String -> String
capitalize s = (toUpper $ take 1 s) <> (drop 1 s)
