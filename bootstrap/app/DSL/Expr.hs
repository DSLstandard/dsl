module DSL.Expr where

import DSL.Location
import DSL.PrettyUtils
import Prettyprinter qualified as PP
import Relude
import qualified Data.Vector as V

type Sym = Text

data Atom
  = ASym Sym
  | AInt Integer
  | AStr Text
  | AChar Char
  deriving (Show, Eq)

data IExpr = IList (V.Vector IExpr) | IAtom Atom
  deriving (Show, Eq)

data SExpr = SList (V.Vector Expr) | SAtom Atom
  deriving (Show, Eq)

data Expr = Expr
  { loc :: Loc
  , the :: SExpr
  }
  deriving (Show, Eq)

pattern SSym :: Sym -> Expr
pattern SSym sym <- Expr _ (SAtom (ASym sym))

pattern SInt :: Integer -> Expr
pattern SInt int <- Expr _ (SAtom (AInt int))

pattern SChar :: Char -> Expr
pattern SChar ch <- Expr _ (SAtom (AChar ch))

pattern SStr :: Text -> Expr
pattern SStr str <- Expr _ (SAtom (AStr str))

exprStripLoc :: Expr -> IExpr
exprStripLoc = \case
  Expr _ (SList xs) -> IList (fmap exprStripLoc xs)
  Expr _ (SAtom a) -> IAtom a

-- * Pretty

prettyAtom :: Atom -> PP.Doc ann
prettyAtom = \case
  ASym s -> PP.pretty s
  AInt n -> PP.pretty n
  AStr s -> PP.viaShow s
  AChar c -> PP.viaShow c

prettyExpr :: Expr -> PP.Doc ann
prettyExpr = prettyIExpr . exprStripLoc

prettyIExpr :: IExpr -> PP.Doc ann
prettyIExpr = \case
  IAtom atom -> prettyAtom atom
  IList items -> slistSep (fmap prettyIExpr items)
