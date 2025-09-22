module DSL.Parser where

import DSL.Expr
import DSL.Location
import DSL.Lexer
import DSL.Token
import Data.DList qualified as DList
import Data.Tuple qualified as Tuple
import Data.Vector qualified as V
import Optics
import Prettyprinter qualified as PP
import Relude

data SyntaxError = SyntaxError
  { loc :: !Loc
  , message :: PP.Doc Void
  }
  deriving (Show)

data PState = PState
  { fileLoc :: !Loc
  , remainingTokens :: ![Token]
  , syntaxErrors :: ![SyntaxError]
  }
  deriving (Show)
makeFieldLabelsNoPrefix ''PState

newtype Parser a = Parser
  { unParser :: State PState a
  }
  deriving newtype (Functor, Applicative, Monad)

runParser :: Loc -> [Token] -> Parser a -> ([SyntaxError], a)
runParser fileLoc tokens parser = do
  let initState = PState{fileLoc, remainingTokens = tokens, syntaxErrors = []}
  let (st', a) = Tuple.swap $ runState (unParser parser) initState
  (st'.syntaxErrors, a)

pushSyntaxError :: SyntaxError -> Parser ()
pushSyntaxError err = Parser do
  modify' $ over #syntaxErrors (err :)

getPState :: Parser PState
getPState = Parser get

setPState :: PState -> Parser ()
setPState = Parser . put

peekToken :: Parser (Maybe Token)
peekToken = do
  st <- getPState
  case st.remainingTokens of
    [] -> pure Nothing
    x : _ -> pure (Just x)

consumeToken :: Parser ()
consumeToken = do
  st <- getPState
  case st.remainingTokens of
    [] -> pure ()
    _ : xs -> setPState (st & set #remainingTokens xs)

nextToken :: Parser (Maybe Token)
nextToken = peekToken <* consumeToken

pList :: Loc -> Loc -> DList.DList Expr -> Parser Expr
pList openLoc lastKnownLoc items = do
  peekToken >>= \case
    Nothing -> do
      pushSyntaxError (SyntaxError openLoc "Closing ']' is absent")
      pure $ Expr (locUnion openLoc lastKnownLoc) (SList (V.fromList $ toList items))
    Just (Token closeLoc TokClose) -> do
      consumeToken
      pure $ Expr (locUnion openLoc closeLoc) (SList (V.fromList $ toList items))
    Just{} -> do
      item <- pExpr
      case item of
        Nothing -> pList openLoc lastKnownLoc items
        Just item -> pList openLoc item.loc (items `DList.snoc` item)

pExpr :: Parser (Maybe Expr)
pExpr = do
  nextToken >>= \case
    Nothing -> do
      st <- getPState
      pushSyntaxError (SyntaxError st.fileLoc "Unexpected end of input")
      pure $ Nothing
    Just (Token atomLoc (TokAtom atom)) -> do
      pure $ Just $ Expr atomLoc (SAtom atom)
    Just (Token openLoc TokOpen) -> do
      expr <- pList openLoc openLoc DList.empty
      pure $ Just expr
    Just (Token loc tok) -> do
      pushSyntaxError (SyntaxError loc ("Unexpected token: " <> show tok))
      pExpr

pMarkRemainingAsSuperfluous :: Parser ()
pMarkRemainingAsSuperfluous = do
  -- Mark the remaining tokens as unexpected
  fix \loop -> do
    nextToken >>= \case
      Nothing -> do
        pass
      Just (Token loc tok) -> do
        pushSyntaxError (SyntaxError loc ("Superfluous token: " <> show tok))
        loop

pWholeFile :: Parser (Maybe Expr)
pWholeFile = do
  expr <- pExpr
  pMarkRemainingAsSuperfluous
  pure expr

data ParseSourceResult = ParseSourceResult
  { syntaxErrors :: ![SyntaxError]
  , expr :: !Expr
  }

parseSource :: Dim -> Text -> ParseSourceResult
parseSource dim source = do
  let
    -- Fan out bad tokens into syntax errors
    (mconcat -> (badTokenSyntaxErrors :: [SyntaxError]), validTokens) =
      flip partitionWith (lexFile dim source) \case
        Token loc (TokBad content) -> do
          Left [SyntaxError loc ("Bad token: " <> show content)]
        Token _loc TokComment -> do
          Left []
        tok -> do
          Right tok

    fileSpan = spanComputeFromFile source
    fileLoc = Loc dim fileSpan

    (parseErrors, exprMaybe) = runParser fileLoc validTokens pWholeFile

    -- If the entire file is faulty, we use a '[]' as a substitute for
    -- implementing error-tolerance
    expr = fromMaybe (Expr fileLoc (SList V.empty)) exprMaybe

  ParseSourceResult
    { syntaxErrors = badTokenSyntaxErrors <> parseErrors
    , expr
    }