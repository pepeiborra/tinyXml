{-# OPTIONS_GHC -fobject-code #-}
{-# LANGUAGE BangPatterns, DisambiguateRecordFields, DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.ByteString.Xml.Internal where

import Control.Applicative
import Control.Arrow (second)
import Control.Exception (try, evaluate, SomeException(..))
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Internal (w2c, ByteString(..))
import Data.Char hiding (Space)
import Data.Array.IArray
import Data.Sequence (Seq)
import Data.Monoid
import Data.Word
import Data.Maybe (isJust)
import Control.Lens
import System.IO.Unsafe
import Foreign.ForeignPtr       (ForeignPtr, withForeignPtr)
import GHC.Stack hiding (SrcLoc)

import Data.ByteString.Xml.Types

newtype ParseState =
  ParseState
    { _cursor :: Str
    }
  deriving Show
makeLenses ''ParseState

type MonadParse m = (MonadState ParseState m)

throw :: ErrorType -> m a
throw etype = error $ show etype 

loc = do
  PS _ o _ <- use cursor
  return (SrcLoc o)

throwLoc :: MonadParse m => (SrcLoc -> ErrorType) -> m a
throwLoc e = loc >>= throw . e

peekAt :: MonadParse m => Int -> m (Char)
peekAt n = use (cursor . to (`BS.unsafeIndex` n) . to w2c)

peek :: MonadParse m => m Char
peek = use (cursor . to BS.unsafeHead . to w2c)

pop :: MonadParse m => m Char
pop = do
  c <- peek
  cursor %= BS.unsafeTail
  return c

find ::MonadParse m => Char -> m (Maybe Str)
find c = do
  bs <- use cursor
  case BS.elemIndex c bs of
    Nothing ->
      return Nothing
    Just i -> do
      (prefix,rest) <- use(cursor . to(BS.splitAt i))
      cursor .= rest
      return (Just prefix)

findPred ::MonadParse m => _ -> m _
findPred pred = do
  bs <- use cursor
  case BS.findIndex pred bs of
    Nothing ->
      return Nothing
    Just ix -> do
      let (prefix, rest) = BS.splitAt ix bs
      cursor .= rest
      return (Just prefix)

trim :: MonadParse m => m _
trim =
  cursor %= BS.dropWhile isSpace
    where
      isSpace c = parseTable ! ord c == Space

expectLoc :: MonadParse m => (Char -> Bool) -> _ -> m Char
expectLoc pred e = do
  c <- pop
  when (not$ pred c) $ throwLoc e
  return c

parseName :: MonadParse m => m Str
parseName = do
  bs <- use cursor
  first <- peek
  case isName1 first of
    False -> return BS.empty
    True  -> do
      let (name, rest) = BS.span isName bs
      cursor .= rest
      return name
 where
    isName1 c = parseTable ! ord c == Name1
    isName c = let x = parseTable ! ord c in x == Name || x == Name1

-- | Assumes cursor is on a '='
parseAttrVal :: MonadParse m => m Str
parseAttrVal = do
  trim
  _ <- expectLoc (== '=') BadAttributeForm
  trim
  c  <- expectLoc (liftA2 (||) (== '\'') (== '\"')) BadAttributeForm
  attrVal <- find c
  case attrVal of
    Just x -> pop *> return x
    Nothing -> throwLoc BadAttributeForm

parseAttrs :: (MonadParse m) => m (Seq Attribute)
parseAttrs = go mempty where
  go acc = do
    trim
    n <- parseName
    if BS.null n
      then return acc
      else do
        v <- parseAttrVal
        go (acc |> Attribute n v)

-- | Parse until </
parseNode :: MonadParse m => m(Node)
parseNode = do
    SrcLoc l <- loc
    _ <- expectLoc (== '<') $ error "parseTag: expected <"
    c <- peek
    when (c == '?') $ cursor %= BS.drop 1
    name  <- parseName
    do -- lazily parse the attributes
        attrs <- parseAttrs
        c <- pop
        n <- peek
        if (c == '/' || c == '?') && n == '>'
          then do
            _ <- pop
            return(Node name l attrs [])
          else do
            unless(c == '>') $ throwLoc (UnterminatedTag name)
            nn <- parseContents
            (next1, next2) <- (,) <$> pop <*> pop
            case (next1, next2) of
              ('<', '/') -> do
                matchTag <- use (cursor . to (name `BS.isPrefixOf`))
                unless matchTag $ throwLoc (ClosingTagMismatch name)
                cursor %= BS.drop (BS.length name)
                _ <- find '>'
                _ <- pop
                return ()
              _ -> throwLoc(UnterminatedTag name)
            return (Node name l attrs nn)

commentEnd :: ByteString -> (ByteString, ByteString)
commentEnd   = BS.breakSubstring $ BS.pack "-->"

dropComments ::MonadParse m => m Bool
dropComments = do
  bs <- use cursor
  case "<!--" `BS.isPrefixOf` bs of
    False ->
      return False
    True ->
      case commentEnd (BS.drop 4 bs) of
        (_,rest) | BS.null rest ->
          throwLoc UnfinishedComment
        (_,rest) -> do
          cursor .= BS.drop 3 rest
          _ <- dropComments
          return True

parseContents :: MonadParse m => m (Seq(Either Str Node))
parseContents = go mempty where
  go acc = do
    trim
    open <- find '<'
    case open of
      Just prefix -> do
        next <- peekAt 1
        case next of
          -- end of tag </
          '/' -> return (appendNonNullPrefix prefix acc)
          _ -> do
            wasComment <- dropComments
            if wasComment
              then
                -- recurse
                go acc
              else do
                n <- parseNode
                go (appendNonNullPrefix prefix acc |> Right n)
      -- no tag
      _ -> do
        rest <- use cursor
        return (appendNonNullPrefix rest acc)
  appendNonNullPrefix bs
      | BS.null bs = id
      | otherwise  = (|> Left bs)

type ParseTable = Array Int ParseType

data ParseType = Name1 | Name | Space | Other deriving (Eq,Show)

-- TODO compute at compile time?
parseTable :: ParseTable
parseTable =
  listArray (0,255)
  [ case chr i of
      ':' -> Name1
      '_' -> Name1
      c | isLetter c -> Name1
      '-' -> Name
      c | isDigit c -> Name
      c | isSpace c -> Space
      _   -> Other
    | i <- [0..255]
    ]

-- Roll our own for efficiency
data ParseResult a = ParseError !Error | ParseSuccess !a
makePrisms ''ParseResult

instance Functor ParseResult where fmap = over _ParseSuccess
instance Applicative ParseResult where
  pure = ParseSuccess
  ParseSuccess pf <*> ParseSuccess px = ParseSuccess (pf px)
  ParseError e <*> _ = ParseError e
  _ <*> ParseError e = ParseError e
instance Monad ParseResult where
  return = pure
  ParseError   e >>= _ = ParseError e
  ParseSuccess x >>= f = f x

newtype ParseMonad a = PM {runPM :: ParseState -> (# a, ParseState #) }
instance Functor ParseMonad where fmap f (PM m) = PM $ \ps -> case m ps of (# a, ps' #) -> (# f a, ps' #)
instance Applicative ParseMonad where
  pure x = PM $ \ps -> (# x, ps #)
  PM pmf <*> PM pmx = PM $ \ps ->
    let  (# f, ps'  #) = pmf ps
         (# x, ps'' #) = pmx ps'
    in (# f x, ps'' #)
instance Monad ParseMonad where
  return = pure
  {-# INLINE (>>=) #-}
  PM m >>= k = PM $ \ps ->
    case m ps of
      (# a, ps' #) -> runPM (k a) ps'
instance MonadState ParseState ParseMonad where
  get   = PM $ \ps -> (# ps, ps #)
  put x = PM $ \_  -> (# (), x #)

parse :: ByteString -> Either String Document
parse b = unsafePerformIO $ do
  res <- try $ evaluate $ case runPM parseContents (ParseState b) of (# it, _ #) -> it
  return$ case res of
    Right it -> Right $ Doc b $ Node "\\" 0 [] it
    Left (SomeException e) -> Left (show e)

(|||) a b c = a || b || c
