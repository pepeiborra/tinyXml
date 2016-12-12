{-# LANGUAGE BangPatterns #-}
module Data.ByteString.Xml
  ( Node(..)
  , Attribute(..)
  , Str
  , Error(..)
  , ErrorType(..)
  , parse
  , children
  ) where

import Data.ByteString.Xml.Internal
import Data.ByteString.Xml.Types
import qualified Data.ByteString as BS
import Control.Lens

{-
childrenBy :: Node -> Str -> [Node]
childrenBy p s = filter (\n -> name n == s) $ children p

attributeBy :: Node -> Str -> Maybe Attribute
attributeBy n s = n ^? attributes.each .filtered(\a -> nameA a == s)

location :: Document -> Node -> (Int, Int)
location Doc{ptr=src} n@(Node _ s _ _) =
  BS.foldl' f (pair 1 1) $ BS.take (fromIntegral s) src
    where
        pair !a !b = (a,b)

        f (!line, !col) c
            | c == '\n' = pair (line+1) 1
            | c == '\t' = pair line (col+8)
            | otherwise = pair line (col+1)

-}
