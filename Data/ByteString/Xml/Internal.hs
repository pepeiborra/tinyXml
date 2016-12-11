{-# LANGUAGE DisambiguateRecordFields, DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
{-# LANGUAGE ViewPatterns #-}

module Data.ByteString.Xml.Internal (parse) where

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
import Control.Lens hiding ((%=),(.=), use) -- redefined locally due to bad inlining
import System.IO.Unsafe
import Foreign.ForeignPtr       (ForeignPtr, withForeignPtr)
import GHC.Stack hiding (SrcLoc)

import Data.ByteString.Xml.Monad
import Data.ByteString.Xml.Types as Str

peekAt :: MonadParse m =>Int -> m (Char)
peekAt n = useE(asBS .to (w2c . (`BS.unsafeIndex` n)))

peek :: MonadParse m => m Char
peek = useE (asBS . to (w2c . BS.unsafeHead))

skip :: MonadParse m => Int -> m ()
skip n = cursor %= Str.drop n

pop :: MonadParse m => m Char
pop = do
  c <- peek
  skip 1
  return c

find ::MonadParse m => Char -> m (Maybe Str)
find c = do
  bs <- useE asBS
  case BS.elemIndex c bs of
    Nothing ->
      return Nothing
    Just i -> do
      prefix <- use(cursor . to(Str.take i))
      skip i
      return (Just prefix)

trim :: MonadParse m => m ()
trim =
  asBS %== BS.dropWhile isSpace
    where
      isSpace c = parseTable ! ord c == Space

expectLoc :: MonadParse m => (Char -> Bool) -> (SrcLoc -> ErrorType) -> m Char
expectLoc pred e = do
  c <- pop
  when (not$ pred c) $ throwLoc e
  return c

parseName :: MonadParse m => m Str
parseName = do
  bs <- useE asBS
  first <- peek
  case isName1 first of
    False -> return strEmpty
    True  -> do
      let (name :: Str, rest) = BS.span isName bs & each %~ view (indexPtr._1)
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
    Just x ->  pop *> return x
    Nothing -> throwLoc BadAttributeForm

parseAttrs :: (MonadParse m) => m (Seq Attribute)
parseAttrs = go mempty where
  go acc = do
    trim
    n <- parseName
    if Str.null n
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
    when (c == '?') (skip 1)
    name  <- parseName
    do -- lazily parse the attributes
        attrs <- parseAttrs
        c <- pop
        n <- peek
        fptr <- ask
        if (c == '/' || c == '?') && n == '>'
          then do
            skip 1
            return(Node name l fptr attrs [])
          else do
            unless(c == '>') $ throwLoc (UnterminatedTag name)
            nn <- parseContents
            (next1, next2) <- (,) <$> pop <*> pop
            case (next1, next2) of
              ('<', '/') -> do
                let n = (name,fptr) ^. from indexPtr
                matchTag <- useE (asBS . to (n `BS.isPrefixOf`))
                unless matchTag $ throwLoc (ClosingTagMismatch name)
                cursor %= Str.drop (Str.length name)
                _ <- find '>'
                skip 1
              _ -> throwLoc(UnterminatedTag name)
            return (Node name l fptr attrs nn)

commentEnd :: ByteString -> (ByteString, ByteString)
commentEnd   = BS.breakSubstring $ BS.pack "-->"

dropComments ::MonadParse m => m Bool
dropComments = do
  bs <- useE asBS
  case "<!--" `BS.isPrefixOf` bs of
    False ->
      return False
    True ->
      case commentEnd (BS.drop 4 bs) of
        (_,rest) | BS.null rest ->
          throwLoc UnfinishedComment
        (_,rest) -> do
          asBS .== BS.drop 3 rest
          _ <- dropComments
          return True

parseContents :: MonadParse m => m (Seq NodeContent)
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
                go (appendNonNullPrefix prefix acc |> NodeChild n)
      -- no tag
      _ -> do
        rest <- use cursor
        return (appendNonNullPrefix rest acc)
  appendNonNullPrefix bs
      | Str.null bs = id
      | otherwise  = (|> NodeText bs)

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

parse :: ByteString -> Either String Node
parse (view indexPtr -> (str,fptr)) = unsafePerformIO $ do
  res <- try $ evaluate $ case runPM parseContents fptr str of (# it, _ #) -> it
  return$ case res of
    Right it ->
      Right $ Node rootStr 0 rootPtr [] it
    Left (SomeException e) ->
      Left (show e)

(|||) a b c = a || b || c

(rootStr, rootPtr) = view indexPtr "\\"
