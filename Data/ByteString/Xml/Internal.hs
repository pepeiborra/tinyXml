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

module Data.ByteString.Xml.Internal (parse) where

import Control.Applicative
import Control.Exception (try, evaluate, SomeException(..))
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.State.Class
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

peekAt :: MonadParse m =>Int -> m (Char)
peekAt n = useE(asBS .to (w2c . (`BS.unsafeIndex` n)))

{-# INLINE peek #-}
peek :: MonadParse m => m (Char)
peek = useE (asBS . to (w2c . BS.unsafeHead))

{-# INLINE skip #-}
skip n = cursor %= Str.drop n

{-# INLINE pop #-}
pop = do
  bs@(PS _ o l) <- useE asBS
  put $ Str (o+1) (l-1)
  return (w2c $ BS.unsafeHead bs)

{-# INLINE find #-}
find c = do
  bs <- useE asBS
  case BS.elemIndex c bs of
    Nothing ->
      return Nothing
    Just i -> do
      let str = bs ^. indexPtr . _1
      let !prefix = Str.take i str
      put $! Str.drop i str
      return (Just prefix)

{-# INLINE trim #-}
trim =
  asBS %== BS.dropWhile isSpace
    where
      isSpace c = parseTable ! ord c == Space

{-# INLINE expectLoc #-}
expectLoc pred e = do
  c <- pop
  when (not$ pred c) $ throwLoc e
  return c

{-# INLINE parseName #-}
parseName = do
  bs <- useE asBS
  let first = w2c $ BS.unsafeHead bs
  case isName1 first of
    False -> return strEmpty
    True  -> do
      let (name :: Str, rest) = BS.span isName bs & each %~ view (indexPtr._1)
      put rest
      return name
 where
    isName1 c = parseTable ! ord c == Name1
    isName c = let x = parseTable ! ord c in x == Name || x == Name1

-- | Assumes cursor is on a '='
{-# INLINE parseAttrVal #-}
parseAttrVal = do
  trim
  _ <- expectLoc (== '=') BadAttributeForm
  trim
  c  <- expectLoc (liftA2 (||) (== '\'') (== '\"')) BadAttributeForm
  attrVal <- find c
  case attrVal of
    Just x ->  skip 1 *> return x
    Nothing -> throwLoc BadAttributeForm

{-# INLINE parseAttrs #-}
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
{-# INLINE parseNode #-}
parseNode = do
    SrcLoc l <- loc
    _ <- expectLoc (== '<') $ error "parseTag: expected <"
    bs0@(PS fptr _ _) <- useE asBS
    bs <- if (w2c (BS.unsafeHead bs0) == '?')
            then do
               let bs = BS.unsafeTail bs0
               put $ view (indexPtr._1) bs
               return bs
            else
               return bs0
    name <- parseName
    do
        attrs <- parseAttrs
        bs <- useE asBS
        let c = w2c $ BS.unsafeHead bs
        let n = w2c $ BS.unsafeIndex bs 1
        if (c == '/' || c == '?') && n == '>'
          then do
            put $ Str.drop 2 $ view (indexPtr._1) bs
            return(Node name l fptr attrs [])
          else do
            unless(c == '>') $ throwLoc (UnterminatedTag name)
            put $ Str.drop 1 $ view (indexPtr._1) bs
            nn <- parseContents
            bs <- useE asBS
            case (w2c$BS.unsafeHead bs, w2c$BS.unsafeIndex bs 1) of
              ('<', '/') -> do
                let n = (name,fptr) ^. from indexPtr
                let matchTag = n `BS.isPrefixOf` BS.unsafeDrop 2 bs
                unless matchTag $ throwLoc (ClosingTagMismatch name)
                put $ Str.drop (Str.length name + 2) $ view (indexPtr._1) bs
                _ <- find '>'
                skip 1
              _ -> throwLoc(UnterminatedTag name)
            return (Node name l fptr attrs nn)

commentEnd :: ByteString -> (ByteString, ByteString)
commentEnd   = BS.breakSubstring $ BS.pack "-->"

{-# INLINE dropComments #-}
dropComments = do
  bs <- useE asBS
  case "<!--" `BS.isPrefixOf` bs of
    False ->
      return False
    True ->
      case commentEnd (BS.drop 4 bs) of
        (_,rest) | BS.null rest ->
          throwLoc UnfinishedComment
        (_, rest) -> do
          put $ Str.drop 3 $ view (indexPtr._1) rest
          _ <- dropComments
          return True

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
parse bs = unsafePerformIO $ do
  res <- try $ evaluate $ runPM bs parseContents
  return$ case res of
    Right it ->
      Right $ Node rootStr 0 rootPtr [] it
    Left (SomeException e) ->
      Left (show e)

(|||) a b c = a || b || c

(rootStr, rootPtr) = view indexPtr "\\"
