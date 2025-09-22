module DSL.Lexer where

import Control.Monad.Reader qualified as Reader
import DSL.Expr
import DSL.Location
import DSL.Token
import Data.Char qualified as Char
import Data.HashSet qualified as HashSet
import Data.Text qualified as T
import Relude
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug qualified as Debug
import Utils.Misc

newtype P a = P
  { unP :: Reader.ReaderT Dim (P.Parsec Void Text) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadFail
    , Alternative
    , MonadPlus
    , P.MonadParsec Void Text
    , Debug.MonadParsecDbg Void Text
    )

scn :: P ()
scn = L.space C.space1 empty empty

pSym :: P Text
pSym = do
  c <- P.satisfy isHeadChar
  cs <- P.takeWhileP Nothing isTailChar
  let sym = T.cons c cs
  pure sym
 where
  isHeadChar :: Char -> Bool
  isHeadChar ch = Char.isAlpha ch || isSpecialChar ch

  isTailChar :: Char -> Bool
  isTailChar ch = Char.isAlphaNum ch || ch == '\'' || isSpecialChar ch

  isSpecialChar :: Char -> Bool
  isSpecialChar ch = HashSet.member ch specialCharSet

  specialCharSet :: HashSet Char
  specialCharSet = HashSet.fromList "!#$%&*+./<=>?@\\^|-~,_:;"

pStringLiteral :: P Text
pStringLiteral = do
  void $ C.char '"'
  cs <- P.manyTill L.charLiteral (C.char '"')
  let str = T.pack cs
  pure str

pCharLiteral :: P Char
pCharLiteral = do
  void $ C.char '\''
  c <- L.charLiteral
  void $ C.char '\''
  pure c

pIntegerLiteral :: P Integer
pIntegerLiteral = do
  L.signed scn L.decimal

pLineComment :: Text -> P Text
pLineComment prefix = do
  -- From L.skipLineComment
  void $ C.string prefix
  P.takeWhileP (Just "character") (/= '\n')

pSkipBlockComment :: Text -> Text -> P Text
pSkipBlockComment start end = do
  void $ C.string start
  T.pack <$> P.manyTill P.anySingle (C.string end)

pTokenData :: P TokenData
pTokenData = do
  asum
    [ do
        void $ C.char '['
        pure $ TokOpen
    , do
        void $ C.char ']'
        pure $ TokClose
    , do
        void $ P.try $ pLineComment "//"
        pure $ TokComment
    , do
        void $ C.string "/*"
        void $ P.manyTill P.anySingle (void P.eof <|> void (C.string "*/"))
        pure $ TokComment
    , do
        ch <- P.try pCharLiteral
        pure $ TokAtom $ AChar ch
    , do
        str <- P.try pStringLiteral
        pure $ TokAtom $ AStr str
    , do
        int <- P.try pIntegerLiteral
        pure $ TokAtom $ AInt int
    , do
        sym <- P.try pSym
        pure $ TokAtom $ ASym sym
    , do
        c <- P.anySingle
        cs <- P.takeWhileP Nothing (\c -> not (c == '[' || c == ']' || Char.isSpace c))
        pure $ TokBad (T.cons c cs)
    ]

pDim :: P Dim
pDim = P Reader.ask

pLocated :: P a -> P (Loc, a)
pLocated parser = do
  dim <- pDim

  start <- toPos <$> P.getSourcePos
  a <- parser
  end <- toPos <$> P.getSourcePos

  pure (Loc dim (Span start end), a)
 where
  toPos :: P.SourcePos -> Pos
  toPos s = Pos (P.unPos s.sourceLine - 1) (P.unPos s.sourceColumn - 1)

pFile :: P [Token]
pFile = do
  scn -- Leading space
  tokens <- P.many $ uncurry Token <$> L.lexeme scn (pLocated pTokenData)
  P.eof
  pure tokens

lexFile :: Dim -> Text -> [Token]
lexFile dim source = do
  case P.runParser (Reader.runReaderT (unP pFile) dim) (show dim) source of
    Left _ -> do
      panic "lexFile: impossible: 'pFile' should never fail"
    Right a -> do
      a