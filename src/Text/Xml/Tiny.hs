{- Module: Text.Xml.Tiny
   Description: A fast DOM parser for a subset of XML. 
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Text.Xml.Tiny
  ( Node(..), name, inner, outer, contents, location
  , Attribute(..)
  , Slice(Slice)
  , SrcLoc(..)
  , Error(..)
  , ErrorType(..)
  , parse, rerender
  , children, childrenBy
  , attributes, attributeBy
  ) where

import Control.Exception
import qualified Text.Xml.Tiny.Internal.Types as Internal
import Text.Xml.Tiny.Types as Slice
import qualified Text.Xml.Tiny.Internal.Parser as Internal
import Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Int
import Data.Maybe (listToMaybe)
import Data.Monoid
import Data.Vector.Storable (Vector, (!))
import qualified Data.Vector.Storable as V
import Foreign (Storable)
import Config
import Text.Printf

-- A parsed XML node
data Node =
  Node{ attributesV :: !(Vector Internal.Attribute)
      , nodesV      :: !(Vector Internal.Node)
      , source      :: !ByteString
      , slices      :: !Internal.Node
      }

instance Show Node where
  show n =
    unlines $
         "Nodes buffer: "
       : [ "  " ++ show n | n <- V.toList $ nodesV n]
       ++ showNodeContents (Right n)
     where
       showNodeContents :: Either ByteString Node -> [String]
       showNodeContents (Right n) =
          [ "Node contents:"
          , "  name: " ++ show (name n)
          , "  slices: " ++ show (slices n)
          , "  attributes: " ++ (show $ attributes n)
          , "  contents: "
          ] ++
          [ "    " ++ l | n' <- contents n, l <- showNodeContents n']
       showNodeContents (Left txt) =
          [ "Text content: " ++ BS.unpack txt ]

-- | A parsed XML attribute
data Attribute = Attribute { attributeName, attributeValue :: !ByteString } deriving (Eq, Show)

-- | Parse an XML bytestring
parse :: Config => ByteString -> Either Error Node
parse bs =
  case Internal.parse bs of
    Left e -> Left e
    Right (attV, nV, slices) -> Right $ Node attV nV bs slices

name, inner, outer :: Config => Node -> ByteString
name  Node{source, slices = Internal.Node{..}} = renderSlice name source
inner Node{source, slices = Internal.Node{..}} = renderSlice inner source
outer Node{source, slices = Internal.Node{..}} = renderSlice outer source

attributes :: Config => Node -> [Attribute]
attributes Node{attributesV, source, slices = Internal.Node{attributes}} =
  [ Attribute (renderSlice n source) (renderSlice v source)
    | Internal.Attribute n v <- vectorSlice attributes attributesV ]

-- | Get the slices of a node, including both the content strings (as 'Left', never blank) and
--   the direct child nodes (as 'Right').
--   If you only want the child nodes, use 'children'.
contents :: Config => Node -> [Either BS.ByteString Node]
contents n@Node{source, slices=Internal.Node{inner}} =
     f (sliceStart inner) (children n)
    where
        f :: Config => Int32 -> [Node] -> [Either BS.ByteString Node]
        f i [] = string i (sliceEnd inner) ++ []
        f i (n@Node{slices=Internal.Node{outer}} : nn) = string i (sliceStart outer) ++ Right n
                                                       : f (sliceEnd outer) nn
        string :: Config => Int32 -> Int32 -> [Either BS.ByteString Node]
        string start end
          | assert (start<=end || error (printf "start=%d, end=%d" start end)) False = undefined
          | start == end = []
          | otherwise = [Left $ renderSlice (fromOpenClose start end) source]

-- | Get the direct child nodes of this node.
children :: Node -> [Node]
children Node{slices = Internal.Node{nodeContents}, ..} =
  [ Node{..} | slices <- vectorSlice nodeContents nodesV ]

-- | Get the direct children of this node which have a specific name.
childrenBy :: Config => Node -> BS.ByteString -> [Node]
childrenBy node str =
  filter (\n -> name n == str) (children node)

-- | Get the first attribute of this node which has a specific name, if there is one.
attributeBy :: Config => Node -> BS.ByteString -> Maybe Attribute
attributeBy node str = listToMaybe [ a | a@(Attribute name _) <- attributes node, name == str ]

-- | Get the (line, col) coordinates of a node w.r.t its original document 
location :: Config => Node -> (Int, Int)
location Node{source, slices=Internal.Node{outer}} =
  BS.foldl' f (pair 1 1) $ BS.take (fromIntegral $ sliceStart outer) source
    where
        pair !a !b = (a,b)
        f (!line, !col) c
            | c == '\n' = pair (line+1) 1
            | c == '\t' = pair line (col+8)
            | otherwise = pair line (col+1)

-- | Returns the XML bytestring reconstructed from the parsed AST. 
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
