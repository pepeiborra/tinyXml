{-# LANGUAGE BangPatterns #-}
module Data.ByteString.Xml
  ( Node(..)
  , Attribute(..)
  , Str
  , Error(..)
  , ErrorType(..)
  , parse
  , children
  , childrenBy
  , attributeBy
  , location
  ) where

import Data.ByteString.Xml.Internal
import Data.ByteString.Xml.Types
import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import Control.Lens
import Control.Monad

childrenBy :: Node -> Str -> [Node]
childrenBy p s = filter (\n -> name n == s) $ children p

attributeBy :: Node -> Str -> Maybe Attribute
attributeBy n s = getAlt $ foldMap (\a -> Alt(a <$ guard(s == nameA a))) $ attributes n
--  listToMaybe $ filter (\(Attribute a _) -> a == s) $ attributes n

location :: Node -> (Int, Int)
location n@(Node _ (Source src) s _ _) =
  BS.foldl' f (pair 1 1) $ BS.take (fromIntegral s) src
    where
        pair !a !b = (a,b)

        f (!line, !col) c
            | c == '\n' = pair (line+1) 1
            | c == '\t' = pair line (col+8)
            | otherwise = pair line (col+1)
