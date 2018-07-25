module Dict (
  Dict(..)
  , get
) where

import Prelude
import Data.Array (find, head, tail)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Traversable (foldr, foldl)

data Dict a = Leaf String a | Branch String (Array (Dict a))

getKey :: forall a. Dict a -> String
getKey (Branch k _nodes) = k
getKey (Leaf k _node) = k

instance showDict :: Show a => Show (Dict a) where
  show (Leaf k v) = "Leaf (" <> k <> " " <> (show v) <> ")"
  show (Branch k lst) =
    "Branch (" <> k <> " " <>
      (foldr (\n acc -> "  " <> (show n) <> "\n" <> acc) "" lst) <>
    ")"

instance functorDict :: Functor Dict where
  map f (Leaf k v) = Leaf k (f v)
  map f (Branch k vs) = Branch k ((map f) <$> vs)

instance eqDict :: Eq a => Eq (Dict a) where
  eq (Leaf k v) (Leaf k' v') = k == k' && v == v'
  eq (Branch k vs) (Branch k' vs') = k == k' && vs == vs'
  eq l r = false

instance applyDict :: Apply Dict where
  apply (Leaf k f) (Leaf _k v) = Leaf k (f v)
  apply (Leaf k l) (Branch _k vs) = Branch k (map (apply (Leaf k l)) vs)
  apply (Branch k fs) r = Branch k ((\f -> apply f r) <$> fs)

get :: forall a. String -> Dict a -> Maybe (Dict a)
get k d
  | getKey d == k = Just d
  | otherwise = do
    ks <- pure $ (split (Pattern ".") k)
    { head: h, tail: t } <- uncons ks
    (foldl (\acc k -> \v -> acc v >>= step k) (get' h) t) d

get' :: forall a. String -> Dict a -> Maybe (Dict a)
get' "" n = Nothing
get' k n
  | getKey n == k = Just n
  | otherwise = Nothing

step :: forall a. String -> (Dict a) -> Maybe (Dict a)
step k (Leaf key v) = if k == key then Just (Leaf key v) else Nothing
step k (Branch _key nodes) = findKey k nodes

findKey :: forall a. String -> Array (Dict a) -> Maybe (Dict a)
findKey k = find (isKey k)

isKey :: forall a. String -> Dict a -> Boolean
isKey k (Leaf key node) = k == key
isKey k (Branch key nodes) = k == key

