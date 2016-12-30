{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
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
import Control.Exception (Exception, try, evaluate, assert)
import Control.Monad
import Control.Monad.State.Class
import Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Internal (w2c, ByteString(..))
import Data.Char hiding (Space)
import Data.Array.Unboxed
import Data.Array.Base as A
import qualified Data.VectorBuilder.Storable as VectorBuilder
import System.IO.Unsafe
import Text.Printf

import Data.ByteString.Xml.Monad
import Data.ByteString.Xml.Internal.Checks
import Data.ByteString.Xml.Internal.Types
import Data.ByteString.Xml.Types as Slice
import Config

default (Int, Double)

{-# INLINE peekAt #-}
peekAt :: Config => Int -> (Char -> Bool) -> ParseMonad s Bool
peekAt n pred = bsIndex n pred

{-# INLINE peek #-}
peek :: Config => (Char -> Bool) -> ParseMonad s Bool
peek pred = bsHead pred

{-# INLINE skip #-}
skip :: (Integral a, Config) => a -> ParseMonad s ()
skip n = modify $ Slice.drop n

{-# INLINE pop #-}
pop :: Config => (Char -> a) -> ParseMonad s a
pop pred = bsHead pred <* skip 1

{-# INLINE findAndPop #-}
findAndPop :: Config => Char -> (SrcLoc -> ErrorType) -> (Slice -> ParseMonad s r) -> ParseMonad s r
findAndPop !c mkE k = do
  !x <- bsElemIndex c
  case x of
    Nothing ->
      throwLoc mkE
    Just !i -> do
      cursor <- get
      let !prefix = Slice.take i cursor
      put(Slice.drop (i+1) cursor)
      k prefix

{-# INLINE trim #-}
trim :: Config => ParseMonad s ()
trim =
  bsDropWhile isSpace
    where
      isSpace !c = spaceTable `unsafeAt` ord c

{-# INLINE expectLoc #-}
expectLoc :: Config => (Char -> Bool) -> _ -> ParseMonad s ()
expectLoc pred e = do
  !b <- peek pred
  unless b $ do
    c <- pop id
    throwLoc(e c)
  skip 1

{-# INLINE parseName #-}
parseName :: Config => ParseMonad s Slice
parseName = {-# SCC "parseTrue" #-} do
  !first <- peek isName1
  case first of
    False -> return sliceEmpty
    True  -> bsSpan isName
 where
    isName1 c = nameTable1 `unsafeAt` ord c
    isName c = nameTable `unsafeAt` ord c

-- | Assumes cursor is on a '='
{-# INLINE parseAttrVal #-}
parseAttrVal :: Config => Slice -> ParseMonad s ()
parseAttrVal n = do
  trim
  expectLoc (== '=') (const BadAttributeForm)
  trim
  !c  <- pop id
  unless (c == '\'' || c == '\"') (throwLoc BadAttributeForm)
  findAndPop c BadAttributeForm (insertAttribute . (Attribute n))

{-# INLINE parseAttr #-}
parseAttr :: Config => ParseMonad s Bool
parseAttr = do
    trim
    !n <-{-# SCC "parseName" #-} parseName
    if Slice.null n
      then return False
      else do
        parseAttrVal n
        return True

parseAttrs :: Config => ParseMonad s Slice
parseAttrs = do
  !initial <- getAttributeBufferCount
  let goParseAttrs = do
        !success <- parseAttr
        when success goParseAttrs
  goParseAttrs
  !current <- getAttributeBufferCount
  return (Slice (fromIntegral initial) (fromIntegral $ current-initial))
{-# INLINE parseAttrs #-}

{-# INLINE parseNode #-}
parseNode :: Config => ParseMonad s ()
parseNode = do
    SrcLoc outerOpen <- loc
    expectLoc (== '<') $ \c -> error( "parseNode: expected < got " ++ [c] )
    !c <- peek (== '?')
    when c (skip 1)
    !name <- {-# SCC "parseName" #-} parseName
    when (Slice.null name) $ throwLoc InvalidNullName
    do
        !attrs <- parseAttrs
        (isTagEnd, isTagClose) <- bsIndex2 0 1 $ \c n -> ((c == '/' || c == '?') && n == '>', c == '>')
        skip 1
        if isTagEnd
          then do
            skip 1
            SrcLoc outerClose <- loc
            let !outer = sliceFromOpenClose outerOpen outerClose
            let inner = sliceEmpty
            _ <- pushNode(Node name inner outer attrs sliceEmpty)
            return ()
          else do
            unless isTagClose $ do
              nameBS <- readStr name
              throwLoc (UnterminatedTag$ BS.unpack nameBS)
            SrcLoc innerOpen <- loc
            me <- pushNode(Node name (sliceFromOpen innerOpen) (sliceFromOpen outerOpen) attrs sliceEmpty)
            !nn <- parseContents
            SrcLoc innerClose <- loc
            isTagEnd <- bsIndex2 0 1 $ \c n -> c == '<' && n == '/'
            skip 2
            !nameBS <- readStr name
            unless isTagEnd $ throwLoc (UnterminatedTag $ BS.unpack nameBS)
            !matchTag <- bsIsPrefix nameBS
            unless matchTag $
              throwLoc (ClosingTagMismatch $ BS.unpack nameBS)
            skip $! Slice.length name
            !bracket <- bsElemIndex '>'
            case bracket of
              Just i  -> skip $! (i+1)
              Nothing -> throwLoc BadTagForm
            SrcLoc outerClose <- loc
            updateNode me $ \n@Node{name=name', inner = Slice innerOpen' _, outer = Slice outerOpen' _, attributes=attrs'} ->
              assert (name==name') $
              assert (innerOpen == fromIntegral innerOpen' || error (printf "Expected:%d, Obtained:%d, me: %d, node: %s" innerOpen innerOpen' me (show n))) $
              assert (outerOpen == fromIntegral outerOpen' || error (printf "Expected:%d, Obtained:%d" outerOpen outerOpen')) $
              Node name' (sliceFromOpenClose innerOpen' innerClose) (sliceFromOpenClose outerOpen' outerClose) attrs' nn

commentEnd :: ParseMonad s ()
{-# INLINE commentEnd #-}
commentEnd  = do
  !end <- bsElemIndex '>'
  case end of
    Nothing -> throwLoc UnfinishedComment
    Just !end -> do
      skip $! (end+1)
      isEnd <- bsIndex2 (-2) (-3) $ \p p' -> p == '-' && p' == '-'
      unless isEnd commentEnd

{-# INLINE dropComments #-}
dropComments :: ParseMonad s Bool
dropComments = do
  !x <- bsIsPrefix "<!--"
  case x of
    False ->
      return False
    True -> do
      !bs <- getBS
      skip 4
      commentEnd
      return True

-- | Returns a slice of nodes
parseContents :: forall s. Config => ParseMonad s Slice
parseContents = do
  !before <- getNodeStackCount
  let goParseContents :: Config => ParseMonad s ()
      goParseContents = do
        !open <- bsElemIndex '<'
        case open of
          Just i -> do
            skip i
            !next <- peekAt 1 (== '/')
            unless next $ do
                !wasComment <- {-# SCC "dropComments" #-} dropComments
                if wasComment
                  then
                    goParseContents
                  else do
                    _ <- parseNode
                    goParseContents
          -- no tag
          _ ->
            return ()
  () <- goParseContents
  !after <- getNodeStackCount
  !front <- getNodeBufferCount
  let diff = after - before
  popNodes diff
  return $! sliceFromOpenClose front (front+diff)

type ParseTable = UArray Int Bool

-- TODO compute at compile time?
spaceTable,nameTable,nameTable1 :: ParseTable
spaceTable = listArray (0,255) [ isSpace (chr i) | i <- [0..255]]

nameTable =
  listArray (0,255)
  [ case chr i of
      ':' -> True
      '_' -> True
      c | isLetter c -> True
      '-' -> True
      c | isDigit c -> True
      _   -> False
    | i <- [0..255]
    ]
nameTable1 =
  listArray (0,255)
  [ case chr i of
      ':' -> True
      '_' -> True
      c | isLetter c -> True
      _   -> False
    | i <- [0..255]
    ]


parse :: Config => ByteString -> Either Error _
parse bs = unsafePerformIO $ do
  res <- try $ evaluate $ runPM bs $ do
    it <- parseContents
    Env{attributeBuffer, nodeBuffer} <- getEnv
    attributesV <- VectorBuilder.finalize attributeBuffer
    nodesV <- VectorBuilder.finalize nodeBuffer
    return $ (it, attributesV, nodesV)
  let len = BS.length bs
  return $! case res of
    Right (!it, attributesV, nodesV) ->
      let rootNode = Node sliceEmpty (sliceFromOpenClose 0 len) (sliceFromOpenClose 0 len) sliceEmpty it
      in Right (attributesV, nodesV, rootNode )
    Left e ->
      Left e
