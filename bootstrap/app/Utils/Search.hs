module Utils.Search where

import Control.Monad.Search qualified as Search
import Relude

newtype Cost = Cost Int
  deriving (Show, Eq, Ord)

unitCost :: Cost
unitCost = Cost 1

instance Semigroup Cost where
  Cost a <> Cost b = Cost (a + b)

instance Monoid Cost where
  mempty = Cost 0

type SearchIO = Search.SearchT Cost IO
