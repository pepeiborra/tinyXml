{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.ByteString.Xml
  ( Node(..)
  , Attribute(..)
  , Slice(Slice)
  , Error(..)
  , ErrorType(..)
  , parse
  , children
  ) where

import Control.Arrow ((&&&))
import qualified Data.ByteString.Xml.Internal.Types as Internal
import Data.ByteString.Xml.Types as Slice
import qualified Data.ByteString.Xml.Internal as Internal
import Data.ByteString.Internal (ByteString(..))
import qualified Data.ByteString.Char8 as BS
import Data.Vector.Storable (Vector, (!))
import qualified Data.Vector.Storable as V
import Foreign (Storable)
import Config

data Node =
  Node{ attributesV :: !(Vector Internal.Attribute)
      , nodesV      :: !(Vector Internal.Node)
      , source      :: !ByteString
      , slices      :: !Internal.Node
      }

instance Show Node where
  show n =
    show (name n, attributes n, contents n)

data Attribute = Attribute { attributeName, attributeValue :: !ByteString } deriving Show

parse :: Config => ByteString -> Either Error Node
parse bs =
  case Internal.parse bs of
    Left e -> Left e
    Right (attV, nV, slices) -> Right $ Node attV nV bs slices

renderSlice :: Slice -> ByteString -> ByteString
renderSlice(Slice o l) (PS fptr _ _) = PS fptr (fromIntegral o) (fromIntegral l)

vectorSlice :: Storable a => Slice -> Vector a -> [a]
vectorSlice s v = [ v ! i | i <- Slice.toList s ]

name, inner, outer :: Node -> ByteString
name  Node{source, slices = Internal.Node{..}} = renderSlice name source
inner Node{source, slices = Internal.Node{..}} = renderSlice inner source
outer Node{source, slices = Internal.Node{..}} = renderSlice outer source

attributes :: Node -> [Attribute]
attributes Node{attributesV, source, slices = Internal.Node{attributes}} =
  [ Attribute (renderSlice n source) (renderSlice v source)
    | Internal.Attribute n v <- vectorSlice attributes attributesV ]


-- | Get the slices of a node, including both the content strings (as 'Left', never blank) and
--   the direct child nodes (as 'Right').
--   If you only want the child nodes, use 'children'.
contents :: Node -> [Either BS.ByteString Node]
contents n@Node{source, slices=Internal.Node{inner}} = f (sliceStart inner) outers
    where
        f i [] = string i (sliceEnd inner) ++ []
        f i ((x, n):xs) = string i (sliceStart x) ++ Right n : f (sliceEnd x) xs

        string start end
          | start == end = []
          | otherwise = [Left $ renderSlice (sliceFromOpenClose start end) source]
        outers = map ((Internal.outer.slices) &&& id) $ children n

-- | Get the direct child nodes of this node.
children :: Node -> [Node]
children Node{slices = Internal.Node{nodeContents}, ..} =
  [ Node{..} | slices <- vectorSlice nodeContents nodesV ]


location :: Node -> (Int, Int)
location Node{source, slices=Internal.Node{outer}} =
  BS.foldl' f (pair 1 1) $ BS.take (fromIntegral $ sliceStart outer) source
    where
        pair !a !b = (a,b)
        f (!line, !col) c
            | c == '\n' = pair (line+1) 1
            | c == '\t' = pair line (col+8)
            | otherwise = pair line (col+1)
