module DSL.Dict where

import DSL.Name
import Data.HashMap.Strict qualified as HashMap
import Relude

newtype Dict a = Dict {unDict :: HashMap.HashMap Name a}
  deriving (Show, Eq, Functor, Foldable, Traversable)

empty :: Dict a
empty = Dict HashMap.empty

insert :: Name -> a -> Dict a -> Dict a
insert name val (Dict m) = Dict (HashMap.insert name val m)

lookup :: Name -> Dict a -> Maybe a
lookup key (Dict m) = HashMap.lookup key m

fromList :: [(Name, a)] -> Dict a
fromList = Dict . HashMap.fromList

toList :: Dict a -> [(Name, a)]
toList (Dict m) = HashMap.toList m

toHashMap :: Dict a -> HashMap.HashMap Name a
toHashMap (Dict m) = m

isAligned :: Dict a -> Dict a -> Bool
isAligned (Dict m1) (Dict m2) =
  HashMap.keysSet m1 == HashMap.keysSet m2

zipWith :: (a -> b -> c) -> Dict a -> Dict b -> Dict c
zipWith f (Dict m1) (Dict m2) = Dict $ HashMap.intersectionWith f m1 m2

zip :: Dict a -> Dict b -> Dict (a, b)
zip = DSL.Dict.zipWith (,)
