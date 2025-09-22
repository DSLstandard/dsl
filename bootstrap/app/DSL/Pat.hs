{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

module DSL.Pat where

import DSL.Expr
import DSL.PrettyUtils (slistSep)
import Data.DList qualified as DList
import Data.List qualified as List
import Data.Vector qualified as V
import Optics
import Prettyprinter qualified as PP
import Relude

newtype PError = PError
  { unPError :: DList.DList (Expr, PP.Doc Void)
  }
  deriving newtype (Semigroup, Monoid)
  deriving (Show)

perrorOne :: Expr -> PP.Doc Void -> PError
perrorOne expr msg =
  PError (DList.singleton (expr, msg))

perrorNil :: PError
perrorNil =
  mempty

perrorList :: PError -> [(Expr, PP.Doc Void)]
perrorList (PError dl) =
  DList.toList dl

type Consumed = Bool

data PExpr a = PExpr
  { formatted :: DList.DList (PP.Doc Void)
  -- ^ List of alternatives
  , recognizeExpr ::
      forall r.
      Expr ->
      PError ->
      Consumed ->
      (Consumed -> Either PError a -> r) ->
      r
  }
makeFieldLabelsNoPrefix ''PExpr

data PList a = PList
  { formatted :: DList.DList (PP.Doc Void)
  -- ^ List of item pieces.
  , recognizeList ::
      forall r.
      Expr ->
      V.Vector Expr ->
      PError ->
      Int ->
      (Int -> Either PError a -> r) ->
      r
  }
makeFieldLabelsNoPrefix ''PList

instance Functor PExpr where
  fmap f (PExpr fmt recognize) =
    PExpr
      { formatted = fmt
      , recognizeExpr = \expr err consumed cont ->
          recognize expr err consumed (\consumed result -> cont consumed (fmap f result))
      }

instance Semigroup (PExpr a) where
  PExpr fmt1 r1 <> PExpr fmt2 r2 =
    PExpr
      { formatted = fmt1 <> fmt2
      , recognizeExpr = \expr err consumed cont ->
          r1 expr perrorNil False \consumed1 -> \case
            Right a -> cont consumed1 (Right a)
            Left err1 ->
              if consumed1
                then do
                  cont True (Left err1)
                else do
                  r2 expr (err <> err1) consumed cont
      }

instance Monoid (PExpr a) where
  mempty =
    PExpr
      { formatted = DList.empty
      , recognizeExpr = \_expr err consumed cont -> cont consumed (Left err)
      }

instance Functor PList where
  fmap a2b (PList fmt r) =
    PList
      { formatted = fmt
      , recognizeList = \self items err i cont -> do
          r self items err i \i -> cont i . fmap a2b
      }

instance Applicative PList where
  pure a =
    PList
      { formatted = DList.empty
      , recognizeList = \_self _items _err i cont -> do
          cont i (Right a)
      }

  (<*>) :: forall a b. PList (a -> b) -> PList a -> PList b
  (<*>) (PList fmt1 r1) (PList fmt2 r2) =
    PList
      { formatted = fmt1 <> fmt2
      , recognizeList = \self items err i cont -> do
          r1 self items err i \i -> \case
            Left err -> cont i (Left err)
            Right a2b -> r2 self items err i \i res -> cont i (fmap a2b res)
      }

instance Alternative PList where
  empty =
    PList
      { formatted = DList.empty
      , recognizeList = \_self _items err i cont -> cont i (Left err)
      }
  PList fmt1 r1 <|> PList fmt2 r2 =
    PList
      { formatted = fmt1 <> fmt2
      , recognizeList = \self items err i cont ->
          r1 self items err i \i' -> \case
            Right a -> cont i' (Right a)
            Left err -> r2 self items err i cont
      }

instance (Semigroup a) => Semigroup (PList a) where
  (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (PList a) where
  mempty = pure mempty

try :: PList a -> PList a
try (PList fmt r) =
  PList
    { formatted = fmt
    , recognizeList = \self items err i cont -> do
        r self items err i \i' -> \case
          Right a ->
            cont i' (Right a)
          Left err ->
            -- Pretend we didn't consumed any items like some LL(k) parser
            -- combinators do. The continuation can tell.
            cont i (Left err)
    }

match :: Expr -> PExpr a -> Either PError a
match expr pexpr = do
  recognizeExpr pexpr expr perrorNil False \_consumed -> Relude.id

matchList :: Expr -> V.Vector Expr -> PList a -> Either PError a
matchList self items plist = do
  recognizeList plist self items perrorNil 0 \i result -> do
    let badFormat = perrorOne self ("Expression format should be: " <> formatList plist)
    case result of
      Left err -> Left (err <> badFormat)
      Right a ->
        if i == V.length items
          then Right a
          else Left badFormat

testExpr :: PP.Doc Void -> (Expr -> Maybe a) -> PExpr a
testExpr formatted recognize =
  PExpr
    { formatted = DList.singleton formatted
    , recognizeExpr = \expr err consumed cont ->
        case recognize expr of
          Nothing -> do
            cont consumed $ Left (err <> perrorOne expr ("Cannot match expression format: " <> formatted))
          Just a -> do
            cont True $ Right a
    }

label :: PP.Doc Void -> PExpr a -> PExpr a
label lbl pexpr =
  pexpr & set #formatted (DList.singleton lbl)

labels :: PP.Doc Void -> PList a -> PList a
labels lbl pexpr =
  pexpr & set #formatted (DList.singleton lbl)

formatList :: PList a -> PP.Doc ann
formatList plist = vacuous $ slistSep plist.formatted

formatExpr :: PExpr a -> PP.Doc ann
formatExpr pexpr = vacuous $ altSep (toList pexpr.formatted)
 where
  altSep :: [PP.Doc ann] -> PP.Doc ann
  altSep choices =
    "{" <> PP.hsep (List.intersperse "|" choices) <> "}"

anySym :: PExpr Sym
anySym = testExpr "<sym>" \case
  Expr _ (SAtom (ASym sym)) -> Just sym
  _ -> Nothing

sym :: Sym -> PExpr ()
sym expected = testExpr (PP.pretty expected) \case
  Expr _ (SAtom (ASym sym)) | sym == expected -> Just ()
  _ -> Nothing

anyStr :: PExpr Text
anyStr = testExpr "<str>" \case
  Expr _ (SAtom (AStr str)) -> Just str
  _ -> Nothing

anyInt :: PExpr Integer
anyInt = testExpr "<int>" \case
  Expr _ (SAtom (AInt i)) -> Just i
  _ -> Nothing

anyChar :: PExpr Char
anyChar = testExpr "<char>" \case
  Expr _ (SAtom (AChar c)) -> Just c
  _ -> Nothing

anyExpr :: PExpr Expr
anyExpr = testExpr "<expr>" Just

captures :: PExpr (Expr -> a) -> PExpr a
captures pexpr =
  PExpr
    { recognizeExpr = \expr err consumed cont ->
        recognizeExpr pexpr expr err consumed \consumed result ->
          cont consumed (($ expr) <$> result)
    , formatted = pexpr.formatted
    }

capture :: PExpr a -> PExpr (Expr, a)
capture pexpr = captures (pexpr <&> \a expr -> (expr, a))

capture_ :: PExpr a -> PExpr Expr
capture_ pexpr = captures (pexpr <&> \_ expr -> expr)

-- | The function should be named 'tryList'
list :: PList a -> PExpr a
list plist =
  PExpr
    { formatted = DList.singleton (formatList plist)
    , recognizeExpr = \expr err consumedIn cont -> do
        let badFormat = perrorOne expr ("Expression format should be: " <> formatList plist)
        case expr of
          Expr _ (SList items) ->
            recognizeList plist expr items err 0 \i result -> do
              let consumed = consumedIn || (i > 0)
              case result of
                Left err -> do
                  cont consumed (Left err)
                Right a -> do
                  if i == V.length items
                    then cont consumed (Right a)
                    else cont consumed (Left badFormat)
          _ -> do
            cont consumedIn $ Left (err <> badFormat)
    }

item :: PExpr a -> PList a
item pexpr =
  PList
    { formatted = DList.singleton (formatExpr pexpr)
    , recognizeList = \self items err i cont ->
        case items V.!? i of
          Nothing -> do
            cont i (Left (err <> perrorOne self ("Missing list expression item: " <> formatExpr pexpr)))
          Just expr ->
            recognizeExpr pexpr expr err False (\consumed -> cont (if consumed then i + 1 else i))
    }

data BlockInfo = BlockInfo
  { self :: Expr
  , leader :: Expr
  }
  deriving (Show)

block :: Sym -> PList (BlockInfo -> a) -> PExpr a
block leader args =
  captures . list $
    (\leader f self -> f (BlockInfo{self, leader}))
      <$> try (item (capture_ (sym leader)))
      <*> args

block_ :: Sym -> PList a -> PExpr a
block_ leader args = block leader (args <&> \a _blockInfo -> a)

manyOf :: forall a. PList a -> PList [a]
manyOf p =
  -- NOTE: Using Alternative's 'many' causes <<loop>>
  PList
    { formatted = do
        DList.singleton (formatList p <> "*")
    , recognizeList = \self items err inI cont -> do
        let
          go (i :: Int) (accum :: DList.DList a) = do
            recognizeList p self items err i \i -> \case
              Right a -> go i (accum `DList.snoc` a)
              Left _err -> cont i (Right (DList.toList accum))
        go inI DList.empty
    }

someOf :: forall a. PList a -> PList (NonEmpty a)
someOf p =
  labels (formatList p <> "+") $
    (:|) <$> p <*> manyOf p

optionalOf :: PList a -> PList (Maybe a)
optionalOf p =
  labels (formatList p <> "?") $
    optional p

manyItem :: PExpr a -> PList [a]
manyItem p = manyOf (item p)

someItem :: PExpr a -> PList (NonEmpty a)
someItem p = someOf (item p)

optionalItem :: PExpr a -> PList (Maybe a)
optionalItem p = optionalOf (item p)

modifier :: PExpr a -> PList Bool
modifier p = isJust <$> optionalItem p

rest :: PList [Expr]
rest = labels "..." (manyItem anyExpr)

rest_ :: PList ()
rest_ = rest $> ()
