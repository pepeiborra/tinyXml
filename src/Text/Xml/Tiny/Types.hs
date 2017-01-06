{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}

-- | This module contains the Slice type for representing subsets of vectors,
--   and the Error type used for modelling XML validation errors.
module Text.Xml.Tiny.Types
  ( Slice(..)
  , fromOpen, fromOpenClose, fromIndexPtr
  , sliceEmpty
  , sliceStart, sliceEnd
  , take, drop, null
  , vectorSlice, renderSlice, toList
  , SrcLoc(..), Error(..), ErrorType(..)
  ) where

import Control.Exception
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Internal (ByteString(..))
import Data.List (genericTake)
import Data.Vector.Storable (Vector, (!))
import qualified Data.Vector.Storable as V
import Data.Word
import GHC.Stack hiding (SrcLoc)
import Foreign.ForeignPtr       (ForeignPtr)
import Foreign
import Prelude hiding (length, null, take, drop)

import Text.Printf

import Config

-- * Slices

-- A subset of a vector defined by an offset and length
data Slice =
  Slice { offset, length :: !Int32 }
  deriving (Eq,Ord,Show)

sInt :: Int
sInt = sizeOf(0::Int32)

instance Storable Slice where
    sizeOf _ = sInt * 2
    alignment _ = alignment (0 :: Int64)
    peek p = Slice <$> peekByteOff p 0 <*> peekByteOff p sInt
    poke p Slice{..} = pokeByteOff p 0 offset >> pokeByteOff p sInt length

{-# INLINE sliceEmpty #-}
sliceEmpty :: Slice
sliceEmpty = Slice 0 0

fromOpen:: Config => Integral a => a -> Slice
fromOpen o = Slice (fromIntegral o) 0

{-# INLINE fromOpenClose #-}
fromOpenClose :: Config => (Integral a1, Integral a) => a -> a1 -> Slice
fromOpenClose (fromIntegral->open) (fromIntegral->close) = Slice open (close-open)

{-# INLINE null #-}
null :: Config => Slice -> Bool
null (Slice _ l) = l == 0

{-# INLINE take #-}
take :: Config => Integral a => a -> Slice -> Slice
take !(fromIntegral -> i) (Slice o l) = assert (l>=i) $ Slice o i

{-# INLINE drop #-}
drop :: Config => Integral t => t -> Slice -> Slice
drop !(fromIntegral -> i) (Slice o l) = assert (l>=i || error(printf "drop %d (Slice %d %d)" i o l)) $ Slice (o+i) (l-i) 

-- | Inclusive
sliceStart :: Config => Slice -> Int32
sliceStart = offset

-- | Non inclusive
sliceEnd :: Config => Slice -> Int32
sliceEnd (Slice o l) = o + l

-- | Returns a list of indexes
toList :: Config => Slice -> [Int]
toList (Slice o l) = genericTake l [ fromIntegral o ..]

{-# INLINE fromIndexPtr #-}
-- | Apply a slice to a foreign ptr of word characters and wrap as a bytestring
fromIndexPtr :: Config => Slice -> ForeignPtr Word8 -> ByteString
fromIndexPtr (Slice o l) fptr = PS fptr (fromIntegral o) (fromIntegral l)

-- | Apply a slice to a bytestring
renderSlice :: Config => Slice -> ByteString -> ByteString
renderSlice(Slice o l) _ | trace (printf "Render slice %d %d" o l) False = undefined
renderSlice(Slice o l) _ | assert (o >= 0 && l >= 0) False = undefined
renderSlice(Slice o l) (PS fptr _ _) = PS fptr (fromIntegral o) (fromIntegral l)

-- | Apply a slice to a vector
vectorSlice :: Config => Storable a => Slice -> Vector a -> [a]
vectorSlice s v = [ v ! i
                  | i <- toList s
                  , assert (i < V.length v) True ]

-- * Error types

newtype SrcLoc = SrcLoc Int deriving Show

data Error = Error ErrorType CallStack
data ErrorType =
    UnterminatedComment SrcLoc
  | UnterminatedTag String SrcLoc
  | ClosingTagMismatch String SrcLoc
  | JunkAtTheEnd Slice SrcLoc
  | UnexpectedEndOfStream
  | BadAttributeForm SrcLoc
  | BadTagForm SrcLoc
  | UnfinishedComment SrcLoc
  | Garbage SrcLoc
  | InvalidNullName SrcLoc
   deriving Show

#if __GLASGOW_HASKELL__ < 800
prettyCallStack = show
#endif

instance Exception Error

instance Show Error where
  show (Error etype cs) = show etype ++ prettyCallStack cs
