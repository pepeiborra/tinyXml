{-# LANGUAGE TemplateHaskell #-}
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

import Control.Lens
import Control.Exception
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Internal (ByteString(..))
import Data.List (genericTake)
import Data.Vector.Storable (Vector)
import Data.Word
import GHC.Stack hiding (SrcLoc)
import Foreign.ForeignPtr       (ForeignPtr, withForeignPtr)
import Foreign.C.Types
import Foreign

import Config

data Slice =
  Slice { offset :: {-# UNPACK #-} !Int32, length :: {-# UNPACK #-} !Int32 }
  deriving (Eq,Show)

instance Storable Slice where
    sizeOf _ = 8
    alignment _ = alignment (0 :: Int64)
    peek p = Slice <$> peekByteOff p 0 <*> peekByteOff p 4
    poke p Slice{..} = pokeByteOff p 0 offset >> pokeByteOff p 4 length

{-# INLINE sliceEmpty #-}
sliceEmpty :: Slice
sliceEmpty = Slice 0 0
sliceFromOpen o = Slice (fromIntegral o) 0
sliceFromOpenClose (fromIntegral->open) (fromIntegral->close) = Slice open (close-open)
{-# INLINE null #-}
null (Slice _ l) = l == 0
{-# INLINE take #-}
take i (Slice o _l) = Slice o (fromIntegral i) -- unsafe
{-# INLINE drop #-}
drop i' (Slice o l) = let i = fromIntegral i' in Slice (o+i) (l-i) -- unsafe

sliceStart s = offset s
sliceEnd (Slice o l) = o + l

toList :: Slice -> [Int]
toList (Slice o l) = genericTake l [ fromIntegral o ..]

{-# INLINE indexPtr #-}
indexPtr :: Iso' ByteString (Slice, ForeignPtr Word8)
indexPtr = iso fromBS toBS where
  fromBS (PS fptr o l) = (Slice (fromIntegral o) (fromIntegral l), fptr)
  toBS (Slice o l, fptr) = PS fptr (fromIntegral o) (fromIntegral l)

{- Error types -}

newtype SrcLoc = SrcLoc Int deriving Show

data Error = Error !ErrorType !CallStack
data ErrorType =
    UnterminatedComment !SrcLoc
  | UnterminatedTag !String !SrcLoc
  | ClosingTagMismatch !String !SrcLoc
  | JunkAtTheEnd !Slice !SrcLoc
  | UnexpectedEndOfStream
  | BadAttributeForm !SrcLoc
  | BadTagForm !SrcLoc
  | UnfinishedComment !SrcLoc
  | Garbage !SrcLoc
   deriving Show

#if __GLASGOW_HASKELL__ < 800
prettyCallStack = show
#endif

instance Exception Error

instance Show Error where
  show (Error etype cs) = show etype ++ prettyCallStack cs

