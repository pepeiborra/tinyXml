{-# LANGUAGE DisambiguateRecordFields, DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.ByteString.Xml.Internal (parse) where

import Control.Applicative
import Control.Exception (try, evaluate, SomeException(..))
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.Maybe (isJust)
import Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Internal (w2c, ByteString(..))
import Data.Char hiding (Space)
import Data.Array.IArray
import Data.Sequence (Seq)
import Control.Lens hiding ((%=),(.=), use) -- redefined locally due to bad inlining
import System.IO.Unsafe

import Data.ByteString.Xml.Monad
import Data.ByteString.Xml.Types as Str

peekAt n = fmap w2c $ bsIndex n

{-# INLINE peek #-}
peek = fmap w2c bsHead

{-# INLINE skip #-}
skip n = cursor %= Str.drop n

{-# INLINE pop #-}
pop = do
  c <- bsHead
  skip 1
  return (w2c c)

{-# INLINE find #-}
find !c  = do
  x <- bsElemIndex c
  case x of
    Nothing ->
      return Nothing
    Just !i -> do
      !prefix <- gets (Str.take i)
      cursor %= Str.drop i
      return $ Just prefix

{-# INLINE trim #-}
trim =
  bsDropWhile isSpace
    where
      isSpace !c = parseTable ! ord c == Space

{-# INLINE expectLoc #-}
expectLoc pred e = do
  !c <- pop
  when (not$ pred c) $ throwLoc(e c)
  return c

{-# INLINE parseName #-}
parseName = do
  !first <- peek
  case isName1 first of
    False -> return strEmpty
    True  -> do
      !name <- bsDropWhile isName
      return name
 where
    isName1 c = parseTable ! ord c == Name1
    isName c = let x = parseTable ! ord c in x == Name || x == Name1

-- | Assumes cursor is on a '='
{-# INLINE parseAttrVal #-}
parseAttrVal = do
  trim
  _ <- expectLoc (== '=') (const BadAttributeForm)
  trim
  !c  <- expectLoc (liftA2 (||) (== '\'') (== '\"')) (const BadAttributeForm)
  attrVal <- find c
  case attrVal of
    Just x ->  skip 1 *> return x
    Nothing -> throwLoc BadAttributeForm

{-# INLINE parseAttrs #-}
parseAttrs = go Empty where
  go acc = do
    trim
    !n <- parseName
    if Str.null n
      then return acc
      else do
        !v <- parseAttrVal
        go (acc |> Attribute n v)

-- | Parse until </
{-# INLINE parseNode #-}
parseNode = do
    _ <- expectLoc (== '<') $ \c -> error( "parseNode: expected < got " ++ [c] )
    !c <- peek
    when (c == '?') (skip 1)
    !name <- parseName
    do
        !attrs <- parseAttrs
        !c <- pop
        !n <- peek
        if (c == '/' || c == '?') && n == '>'
          then do
            skip 1
            return(ElementData name attrs Empty)
          else do
            !fptr <- ask
            let !nameStr = (name,fptr) ^. from indexPtr
            unless(c == '>') $
              throwLoc (UnterminatedTag$ BS.unpack nameStr)
            !nn <- parseContents
            !c <- pop
            !n <- pop
            case (c,n) of
              ('<', '/') -> do
                !matchTag <- bsIsPrefix nameStr
                unless matchTag $ throwLoc (ClosingTagMismatch $ show nameStr)
                skip (Str.length name)
                !bracket <- find '>'
                skip 1
                unless(isJust bracket) $ throwLoc BadTagForm
              _ -> throwLoc(UnterminatedTag $ BS.unpack nameStr)
            return (ElementData name attrs nn)

commentEnd :: ByteString -> (ByteString, ByteString)
{-# INLINE commentEnd #-}
commentEnd   = BS.breakSubstring $ BS.pack "-->"

{-# INLINE dropComments #-}
dropComments = do
  x <- bsIsPrefix "<!--"
  case x of
    False ->
      return False
    True -> do
      bs <- useE asBS
      case commentEnd (BS.unsafeDrop 4 bs) of
        (_,rest) | BS.null rest ->
          throwLoc UnfinishedComment
        (_, rest) -> do
          put $! Str.drop 3 $ view (indexPtr._1) rest
          _ <- dropComments
          return True

parseContents = do
  fptr <- ask
  let appendNonNullPrefix bs
       | Str.null bs = id
       | otherwise  = (|> Node fptr (Text bs))
  let go acc = do
        trim
        !open <- find '<'
        case open of
          Just !prefix -> do
            !next <- peekAt 1
            case next of
              -- end of tag </
              '/' -> return (appendNonNullPrefix prefix acc)
              _ -> do
                !wasComment <- dropComments
                if wasComment
                  then
                    -- recurse
                    go acc
                  else do
                    !n <- parseNode
                    go (appendNonNullPrefix prefix acc |> Node fptr (Element n))
          -- no tag
          _ -> do
            rest <- use cursor
            return (appendNonNullPrefix rest acc)
  go Empty


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
parse bs = unsafePerformIO $ do
  res <- try $ evaluate $ runPM bs parseContents
  return$ case res of
    Right it ->
      Right $ Node rootPtr (Element (ElementData rootStr Empty it))
    Left (SomeException e) ->
      Left (show e)

(|||) a b c = a || b || c

(rootStr, rootPtr) = view indexPtr "\\"
