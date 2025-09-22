module DSL.Location where

import Data.Text qualified as T
import Prettyprinter qualified as P
import Relude
import Text.Show qualified

-- * Pos

data Pos = Pos {line :: !Int, col :: !Int}
  deriving (Show, Eq)

posIsBefore :: Pos -> Pos -> Bool
posIsBefore a b = (a.line < b.line) || (a.line == b.line && a.col < b.col)

posIsAfter :: Pos -> Pos -> Bool
posIsAfter a b = posIsBefore b a

instance Ord Pos where
  compare a b
    | posIsBefore a b = LT
    | posIsAfter a b = GT
    | otherwise = EQ

posMin :: Pos -> Pos -> Pos
posMin = min

posMax :: Pos -> Pos -> Pos
posMax = max

posMoveRight :: Pos -> Int -> Pos
posMoveRight (Pos line col) n = Pos line (col + n)

posAdvance :: Char -> Pos -> Pos
posAdvance '\n' (Pos l _) = Pos (l + 1) 0
posAdvance _ (Pos l c) = Pos l (c + 1)

posComputeFromIndex :: Text -> Int -> Pos
posComputeFromIndex inSource inIndex = do
  let prefix = T.take inIndex inSource
  T.foldl' (flip posAdvance) posFileStart prefix

posFileStart :: Pos
posFileStart = Pos 0 0

posComputeFileEnd :: Text -> Pos
posComputeFileEnd source = posComputeFromIndex source (T.length source)

-- * Span

data Span = Span {start :: !Pos, stop :: !Pos}
  deriving (Show, Eq)

spanUnion :: Span -> Span -> Span
spanUnion (Span s1 e1) (Span s2 e2) =
  Span (posMin s1 s2) (posMax e1 e2)

spanComputeFromFile :: Text -> Span
spanComputeFromFile source = Span posFileStart (posComputeFileEnd source)

spanMoveRight :: Span -> Int -> Span
spanMoveRight (Span start stop) n = Span (posMoveRight start n) (posMoveRight stop n)

spanSingleton :: Pos -> Span
spanSingleton pos = Span pos pos

-- * Dimension

newtype Dim = Dim {unDim :: Text}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Hashable, IsString)

-- * Location

data Loc = Loc {dim :: !Dim, span :: !Span}
  deriving (Eq)

prettyLoc :: Loc -> P.Doc ann
prettyLoc (Loc dim span) = P.pretty (unDim dim) <> ":" <> prettySpan span
 where
  prettySpan :: Span -> P.Doc ann
  prettySpan (Span start stop) = prettyPos start <> "-" <> prettyPos stop

  prettyPos :: Pos -> P.Doc ann
  prettyPos pos = "(" <> P.pretty (pos.line + 1) <> "," <> P.pretty (pos.col + 1) <> ")"

locUnion :: Loc -> Loc -> Loc
locUnion a b = Loc a.dim (spanUnion a.span b.span)

instance Show Loc where
  -- For better debuggability
  show = show . prettyLoc

-- * User cursor

data CursorPos = CursorPos
  { dim :: !Dim
  , pos :: !Pos
  }
  deriving (Show)

-- * Utils

spanIsStart :: Span -> Pos -> Bool
spanIsStart (Span start _) pos = start == pos

spanIsEnd :: Span -> Pos -> Bool
spanIsEnd (Span _ stop) pos = stop == pos

posIsBeforeSpan :: Pos -> Span -> Bool
posIsBeforeSpan pos (Span start _) = pos < start

posIsAfterSpan :: Pos -> Span -> Bool
posIsAfterSpan pos (Span _ stop) = pos > stop

{- | Checks if the given position is within the span. Exclusive of start and
stop.
-}
spanSubsumes :: Span -> Pos -> Bool
spanSubsumes (Span start stop) pos =
  posIsAfter pos start && posIsBefore pos stop

-- | Like 'spanSubsumes', but inclusive of start and exclusive of stop.
spanIsInBounds :: Span -> Pos -> Bool
spanIsInBounds (Span start stop) pos =
  posIsAfter pos start && posIsBefore pos stop
    || spanIsStart (Span start stop) pos
    || spanIsEnd (Span start stop) pos

spanInsertionBounds :: Span -> Span
spanInsertionBounds span = spanMoveRight span 1

{- | Checks if the given position is either within the span or at the end of the
span. Exclusive of start.
-}
spanIsInsertion :: Span -> Pos -> Bool
spanIsInsertion span = spanIsInBounds (spanInsertionBounds span)

-- spanIsBeforeInsertion :: Span -> Pos -> Bool
-- spanIsBeforeInsertion

liftTest :: (Span -> Pos -> Bool) -> Loc -> CursorPos -> Bool
liftTest test (Loc locDim locSpan) (CursorPos cursorDim cursorPos) =
  locDim == cursorDim && test locSpan cursorPos
