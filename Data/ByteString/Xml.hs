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
location :: Node -> (Int, Int)
location n@(Node src s) =
  BS.foldl' f (pair 1 1) $ BS.take (fromIntegral s) src
    where
        pair !a !b = (a,b)

        f (!line, !col) c
            | c == '\n' = pair (line+1) 1
            | c == '\t' = pair line (col+8)
            | otherwise = pair line (col+1)

-}
