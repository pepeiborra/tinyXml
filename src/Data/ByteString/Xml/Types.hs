{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}

module Data.ByteString.Xml.Types where

import Control.Exception
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Internal (ByteString(..))
import Data.List (genericTake)
import Data.Vector.Storable (Vector, (!))
import qualified Data.Vector.Storable as V
import Data.Word
import GHC.Stack hiding (SrcLoc)
import Foreign.ForeignPtr       (ForeignPtr, withForeignPtr)
import Foreign.C.Types
import Foreign
import Prelude hiding (length)

import Text.Printf

import Config

data Slice =
  Slice { offset, length :: !Int32 }
  deriving (Eq,Ord,Show)
sInt = sizeOf(0::Int32)

instance Storable Slice where
    sizeOf _ = sInt * 2
    alignment _ = alignment (0 :: Int64)
    peek p = Slice <$> peekByteOff p 0 <*> peekByteOff p sInt
    poke p Slice{..} = pokeByteOff p 0 offset >> pokeByteOff p sInt length

{-# INLINE sliceEmpty #-}
sliceEmpty :: Slice
sliceEmpty = Slice 0 0
sliceFromOpen o = Slice (fromIntegral o) 0
sliceFromOpenClose (fromIntegral->open) (fromIntegral->close) = Slice open (close-open)
{-# INLINE null #-}
null (Slice _ l) = l == 0
{-# INLINE take #-}
take !i (Slice o _l) = Slice o (fromIntegral i) -- unsafe
{-# INLINE drop #-}
drop !i' (Slice o l) = let !i = fromIntegral i' in Slice (o+i) (l-i) -- unsafe

-- | Inclusive
sliceStart s = offset s
-- | Non inclusive
sliceEnd (Slice o l) = o + l

toList :: Slice -> [Int]
toList (Slice o l) = genericTake l [ fromIntegral o ..]

{-# INLINE fromIndexPtr #-}
fromIndexPtr :: Slice -> ForeignPtr Word8 -> ByteString
fromIndexPtr = curry toBS where
  fromBS (PS fptr o l) = (Slice (fromIntegral o) (fromIntegral l), fptr)
  toBS (Slice o l, fptr) = PS fptr (fromIntegral o) (fromIntegral l)

renderSlice :: Config => Slice -> ByteString -> ByteString
renderSlice(Slice o l) _ | trace (printf "Render slice %d %d" o l) False = undefined
renderSlice(Slice o l) _ | assert (o >= 0 && l >= 0) False = undefined
renderSlice(Slice o l) (PS fptr _ _) = PS fptr (fromIntegral o) (fromIntegral l)

vectorSlice :: Config => Storable a => Slice -> Vector a -> [a]
vectorSlice s v = [ v ! i
                  | i <- toList s
                  , assert (i < V.length v) True ]

{- Error types -}

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

