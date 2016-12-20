{-# LANGUAGE DisambiguateRecordFields #-}
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
{-# LANGUAGE Strict #-}

module Data.ByteString.Xml.Internal(parse) where

import Control.Applicative
import Control.Exception (try, evaluate, assert)
import Control.Monad
import Control.Monad.State.Class
import Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Internal (w2c, ByteString(..))
import Data.Char hiding (Space)
import Data.Array.Unboxed
import Data.Array.Base as A
import qualified Data.VectorBuilder.Storable as VectorBuilder
import Control.Lens hiding ((%=),(.=), use) -- redefined locally due to bad inlining
import System.IO.Unsafe

import Data.ByteString.Xml.Monad
import Data.ByteString.Xml.Internal.Checks
import Data.ByteString.Xml.Internal.Types
import Data.ByteString.Xml.Types as Slice
import Config

{-# INLINE peekAt #-}
peekAt :: Config => _ -> ParseMonad s Char
peekAt n = fmap w2c $ bsIndex n

{-# INLINE peek #-}
peek :: Config => ParseMonad s Char
peek = fmap w2c bsHead

{-# INLINE skip #-}
skip :: (Integral a, Config) => a -> ParseMonad s ()
skip n = cursor %= Slice.drop n

{-# INLINE pop #-}
pop :: Config => ParseMonad s Char
pop = {-# SCC "pop" #-} do
  !c <- bsHead
  skip 1
  return (w2c c)

{-# INLINE find #-}
find :: Config => Char -> ParseMonad s _
find !c  = do
  !x <- bsElemIndex c
  case x of
    Nothing ->
      return Nothing
    Just !i -> do
      !prefix <- Slice.take i <$> get
      skip i
      return $ Just prefix

{-# INLINE trim #-}
trim :: Config => ParseMonad s ()
trim =
  bsDropWhile isSpace
    where
      isSpace !c = spaceTable `unsafeAt` ord c

{-# INLINE expectLoc #-}
expectLoc :: Config => _ -> _ -> ParseMonad s _
expectLoc pred e = do
  !c <- pop
  when (not$ pred c) $ throwLoc(e c)
  return c

{-# INLINE parseName #-}
parseName :: Config => ParseMonad s Slice
parseName = {-# SCC "parseTrue" #-} do
  !first <- peek
  case isName1 first of
    False -> return sliceEmpty
    True  -> bsSpan isName
 where
    isName1 c = nameTable1 `unsafeAt` ord c
    isName c = nameTable `unsafeAt` ord c

-- | Assumes cursor is on a '='
{-# INLINE parseAttrVal #-}
parseAttrVal :: Config => ParseMonad s Slice
parseAttrVal = do
  trim
  _ <- expectLoc (== '=') (const BadAttributeForm)
  trim
  !c  <- expectLoc (liftA2 (||) (== '\'') (== '\"')) (const BadAttributeForm)
  attrVal <- find c
  case attrVal of
    Just !x -> skip 1 *> return x
    Nothing -> throwLoc BadAttributeForm

{-# INLINE parseAttr #-}
parseAttr :: Config => ParseMonad s Bool
parseAttr = do
    trim
    !n <-{-# SCC "parseName" #-} parseName
    if Slice.null n
      then return False
      else do
        !v <- parseAttrVal
        _  <- insertAttribute(Attribute n v)
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
parseNode :: Config => ParseMonad s _
parseNode = do
    SrcLoc outerOpen <- loc
    _ <- expectLoc (== '<') $ \c -> error( "parseNode: expected < got " ++ [c] )
    !c <- peek
    when (c == '?') (skip 1)
    !name <- {-# SCC "parseName" #-} parseName
    assert (not $ Slice.null name) $ return ()
    do
        !attrs <- parseAttrs
        !c <- pop
        !n <- peek
        if (c == '/' || c == '?') && n == '>'
          then do
            skip 1
            SrcLoc outerClose <- loc
            let !outer = sliceFromOpenClose outerOpen outerClose
            let !inner = sliceEmpty
            insertNode(Node name inner outer attrs sliceEmpty)
          else do
            unless(c == '>') $ do
              nameBS <- readStr name
              throwLoc (UnterminatedTag$ BS.unpack nameBS)
            SrcLoc innerOpen <- loc
            pushNode(Node name (sliceFromOpen innerOpen) (sliceFromOpen outerOpen) attrs sliceEmpty)
            !nn <- parseContents
            SrcLoc innerClose <- loc
            !c <- pop
            !n <- pop
            Node name' (Slice io _) (Slice oo _) attrs _ <- popNode
            !nameBS <- readStr name'
            checkConsistency outerOpen innerClose name name'
            case (c,n) of
              ('<', '/') -> do
                !matchTag <- bsIsPrefix nameBS
                unless matchTag $ do
                  throwLoc (ClosingTagMismatch $ BS.unpack nameBS)
                skip $ Slice.length name'
                !bracket <- bsElemIndex '>'
                case bracket of
                  Just i  -> skip (i+1)
                  Nothing -> throwLoc BadTagForm
              _ -> throwLoc(UnterminatedTag $ BS.unpack nameBS)
            SrcLoc outerClose <- loc
            insertNode $ Node name' (sliceFromOpenClose io innerClose) (sliceFromOpenClose oo outerClose) attrs nn

commentEnd :: ByteString -> (ByteString, ByteString)
{-# INLINE commentEnd #-}
commentEnd   = BS.breakSubstring $ BS.pack "-->"

{-# INLINE dropComments #-}
dropComments = do
  !x <- bsIsPrefix "<!--"
  case x of
    False ->
      return False
    True -> do
      !bs <- useE asBS
      case commentEnd (BS.unsafeDrop 4 bs) of
        (_,rest) | BS.null rest ->
          throwLoc UnfinishedComment
        (_, rest) -> do
          put $! Slice.drop 3 $ view (indexPtr._1) rest
          return True

-- | Returns a slice of nodes
parseContents :: forall s. Config => ParseMonad s Slice
parseContents = do
  !start <- getNodeStackCount
  let goParseContents :: Config => ParseMonad s ()
      goParseContents = do
        trim
        !open <- bsElemIndex '<'
        case open of
          Just i -> do
            skip i
            !next <- peekAt 1
            case next of
              -- end of tag </
              '/' -> return ()
              _ -> do
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
  !current <- getNodeStackCount
  return $! sliceFromOpenClose start current

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

(|||) a b c = a || b || c

(rootStr, rootPtr) = view indexPtr "\\"

