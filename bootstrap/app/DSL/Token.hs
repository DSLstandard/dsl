module DSL.Token where

import DSL.Expr
import DSL.Location
import Relude

data TokenData
  = TokAtom Atom
  | TokComment
  | TokBad
      -- | Content
      Text
  | TokOpen
  | TokClose
  deriving (Show, Eq)

data Token = Token
  { loc :: Loc
  , the :: TokenData
  }
  deriving (Show, Eq)
