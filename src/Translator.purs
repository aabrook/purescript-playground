module Translator where

import Prelude
import Data.Array (find, cons, uncons, head, tail, (:), take, drop)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.String (Pattern(..), joinWith, split, toUpper)
import Data.String (drop, take) as S
import Data.Traversable

data Dict = Leaf String String | Branch String (Array Dict)

instance showDict :: Show Dict where
  show (Leaf k v) = "Leaf (" <> k <> " " <> v <> ")"
  show (Branch k lst) =
    "Branch (" <> k <> " " <>
      (foldr (\n acc -> "  " <> (show n) <> "\n" <> acc) "" lst) <>
    ")"

newDict :: Dict
newDict = Leaf "a" "b"

toLeaf :: Tuple String String -> Dict
toLeaf tups = Leaf (fst tups) (snd tups)

addKey :: Tuple String String -> Dict -> Dict
addKey node (Branch k lst) = Branch k (lst <> [toLeaf node])
addKey node (Leaf k s) = Branch k [toLeaf node]

tOrGuess :: String -> Dict -> String
tOrGuess k nodes =
  case t k nodes of
    Just s -> s
    Nothing -> guess k
    where
      guess k = split (Pattern "_") k # map capitalize # joinWith " "

t :: String -> Dict -> Maybe String
t "" node = Nothing
t k (Leaf key s) = if k == key then Just s else Nothing
t k (Branch key []) = Nothing
t k (Branch key lst)
  | k == key = Nothing
  | otherwise = do
    ck <- head (split (Pattern ".") k)
    _k <- if ck /= key || ck == "" then Nothing
          else pure ck
    tl <- tail (split (Pattern ".") k)
    nextKey <- head tl >>= (\k -> if k == "" then Nothing else Just k)
    remainingKey <- joinWith "." <$> tail tl
    node <- findKey nextKey lst
    t (joinWith "." tl) node

findKey :: String -> Array Dict -> Maybe Dict
findKey k = find (isKey k)

isKey :: String -> Dict -> Boolean
isKey k (Leaf key node) = k == key
isKey k (Branch key nodes) = k == key

capitalize :: String -> String
capitalize s = (toUpper $ S.take 1 s) <> (S.drop 1 s)
