{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | This module contains functions for parsing an XML document and deconstructing the AST.
module Text.Xml.Tiny
  ( Node, name, inner, outer, contents, location
  , Attribute(..)
  , parse
  , children, childrenBy
  , attributes, attributeBy
  , SrcLoc(..)
  , Error(..)
  , ErrorType(..)
  , rerender
  ) where

import Control.Exception
import Text.Xml.Tiny.Internal (Node(..), Attribute(..), ParseDetails(ParseDetails), AttributeParseDetails(..), Error(..), ErrorType(..), SrcLoc(..))
import qualified Text.Xml.Tiny.Internal as Slice
import qualified Text.Xml.Tiny.Internal.Parser as Internal
import Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Int
import Data.Maybe (listToMaybe)
import Data.Monoid
import Config
import Text.Printf

-- | Parse an XML bytestring, returning a root node with name "" and the content AST, or an error.
--   Note that the returned AST references the input bytestring and will therefore keep it alive.
parse :: Config => ByteString -> Either Error Node
parse bs =
  case Internal.parse bs of
    Left e -> Left e
    Right (attV, nV, slices) -> Right $ Node attV nV bs slices

instance Show Node where
  -- | Returns a simplified rendering of the node.
  --   If you want a full rendering, use `outer`
  show n =
    case children n of
      [] -> printf "<%s%s/>" nameN attrs
      _  -> printf "<%s%s>...</%s>" nameN attrs nameN
   where
    nameN = BS.unpack $ name n
    attrs = unwords [ BS.unpack n <> "=" <> BS.unpack v | Attribute n v <- attributes n]

name, inner, outer :: Config => Node -> ByteString
-- | The tag name
name  Node{source, slices = ParseDetails{..}} = Slice.render name source
-- | The content of the tag, excluding the tag itself
inner Node{source, slices = ParseDetails{..}} = Slice.render inner source
-- | The contents of the tag, including the tag itself
outer Node{source, slices = ParseDetails{..}} = Slice.render outer source
-- | The attributes in the tag, if any.
attributes :: Config => Node -> [Attribute]
attributes Node{attributesV, source, slices = ParseDetails{attributes}} =
  [ Attribute (Slice.render n source) (Slice.render v source)
    | AttributeParseDetails n v <- Slice.vector attributes attributesV ]

-- | Get the slices of a node, including both the content strings (as 'Left', never blank) and
--   the direct child nodes (as 'Right').
--   If you only want the child nodes, use 'children'.
contents :: Config => Node -> [Either BS.ByteString Node]
contents n@Node{source, slices=ParseDetails{inner}} =
     f (Slice.start inner) (children n)
    where
        f :: Config => Int32 -> [Node] -> [Either BS.ByteString Node]
        f i [] = string i (Slice.end inner) ++ []
        f i (n@Node{slices=ParseDetails{outer}} : nn) = string i (Slice.start outer) ++ Right n
                                                       : f (Slice.end outer) nn
        string :: Config => Int32 -> Int32 -> [Either BS.ByteString Node]
        string start end
          | assert (start<=end || error (printf "start=%d, end=%d" start end)) False = undefined
          | start == end = []
          | otherwise = [Left $ Slice.render (Slice.fromOpenClose start end) source]

-- | Get the direct child nodes of this node.
children :: Node -> [Node]
children Node{slices = ParseDetails{nodeContents}, ..} =
  [ Node{..} | slices <- Slice.vector nodeContents nodesV ]

-- | Get the direct children of this node which have a specific name.
childrenBy :: Config => Node -> BS.ByteString -> [Node]
childrenBy node str =
  filter (\n -> name n == str) (children node)

-- | Get the first attribute of this node which has a specific name, if there is one.
attributeBy :: Config => Node -> BS.ByteString -> Maybe Attribute
attributeBy node str = listToMaybe [ a | a@(Attribute name _) <- attributes node, name == str ]

-- | Get the (line, col) coordinates of a node
location :: Config => Node -> (Int, Int)
location Node{source, slices=ParseDetails{outer}} =
  BS.foldl' f (pair 1 1) $ BS.take (fromIntegral $ Slice.start outer) source
    where
        pair !a !b = (a,b)
        f (!line, !col) c
            | c == '\n' = pair (line+1) 1
            | c == '\t' = pair line (col+8)
            | otherwise = pair line (col+1)

-- | Returns the XML bytestring reconstructed from the parsed AST.
--   For the original XML, use `outer`
rerender :: Node -> BS.ByteString
rerender = inside
    where
        inside x = BS.concat $ map (either validStr node) $ contents x
        node x = "<" <> BS.unwords (validName (name x) : map attr (attributes x)) <> ">" <>
                 inside x <>
                 "</" <> name x <> ">"
        attr (Attribute a b) = validName a <> "=\"" <> validAttr b <> "\""

        validName x | BS.all (\x -> isAlphaNum x || x `elem` ("-:_" :: String)) x = x
                    | otherwise = error "Invalid name"
        validAttr x | BS.notElem '\"' x = x
                    | otherwise = error "Invalid attribute"
        validStr x | BS.notElem '<' x || BS.isInfixOf "<!--" x = x
                   | otherwise = error $ show ("Invalid string", x)
