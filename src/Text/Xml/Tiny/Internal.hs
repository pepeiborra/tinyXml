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

-- | This module contains types and functions that are mostly
--   intended for the test suite, and should be considered internal
--   for all other purposes
module Text.Xml.Tiny.Internal
  (
-- * XML Nodes
    Node(..), Attribute(..)
-- * ParseDetails
  , ParseDetails(..), AttributeParseDetails(..)
-- * Slices
  , Slice(..)
  , fromOpen, fromOpenClose, fromIndexPtr
  , empty
  , start, end
  , take, drop, null
  , vector, render, toList
-- * Errors
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

{-# INLINE empty #-}
empty :: Slice
empty = Slice 0 0

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
start :: Config => Slice -> Int32
start = offset

-- | Non inclusive
end :: Config => Slice -> Int32
end (Slice o l) = o + l

-- | Returns a list of indexes
toList :: Config => Slice -> [Int]
toList (Slice o l) = genericTake l [ fromIntegral o ..]

{-# INLINE fromIndexPtr #-}
-- | Apply a slice to a foreign ptr of word characters and wrap as a bytestring
fromIndexPtr :: Config => Slice -> ForeignPtr Word8 -> ByteString
fromIndexPtr (Slice o l) fptr = PS fptr (fromIntegral o) (fromIntegral l)

-- | Apply a slice to a bytestring
render :: Config => Slice -> ByteString -> ByteString
render(Slice o l) _ | trace (printf "Render slice %d %d" o l) False = undefined
render(Slice o l) _ | assert (o >= 0 && l >= 0) False = undefined
render(Slice o l) (PS fptr _ _) = PS fptr (fromIntegral o) (fromIntegral l)

-- | Apply a slice to a vector
vector :: Config => Storable a => Slice -> Vector a -> [a]
vector s v = [ v ! i
              | i <- toList s
              , assert (i < V.length v) True ]

-- * XML Nodes

-- | A parsed XML node
data Node =
  Node{ attributesV :: !(Vector AttributeParseDetails) -- ^ All the attributes in the document
      , nodesV      :: !(Vector ParseDetails)          -- ^ All the nodes in the document
      , source      :: !ByteString                     -- ^ The document bytes
      , slices      :: !ParseDetails                   -- ^ Details for this node
      }

-- | A parsed XML attribute
data Attribute = Attribute { attributeName, attributeValue :: !ByteString } deriving (Eq, Show)

data ParseDetails =
    ParseDetails
    { name :: {-# UNPACK #-} !Slice     -- ^ bytestring slice
    , inner :: {-# UNPACK #-} !Slice    -- ^ bytestring slice
    , outer :: {-# UNPACK #-} !Slice    -- ^ bytestring slice
    , attributes :: {-# UNPACK #-} !Slice   -- ^ ParseDetailsAttribute slice
    , nodeContents :: {-# UNPACK #-} !Slice -- ^ ParseDetails slice of children
    }
    -- | An incompletely defined set of parse details
  | ProtoParseDetails { name, attributes :: !Slice, innerStart, outerStart :: !Int32 }
  deriving (Show)

-- | Assumes that a name can never be the empty slice
instance Storable ParseDetails where
  sizeOf    _ = sizeOf empty * 5
  alignment _ = alignment(0::Int)
  poke !q (ParseDetails a b c d e)  = let p = castPtr q in pokeElemOff p 0 a >> pokeElemOff p 1 b >> pokeElemOff p 2 c >> pokeElemOff p 3 d >> pokeElemOff p 4 e
  poke !q (ProtoParseDetails (Slice no nl) (Slice ao al) i o) = do
    let !p = castPtr q
    pokeElemOff p 0 no
    pokeElemOff p 1 (0::Int32)
    pokeElemOff p 2 nl
    pokeElemOff p 3 ao
    pokeElemOff p 4 al
    pokeElemOff p 5 i
    pokeElemOff p 6 o
  peek q = do
    let !p = castPtr q
    !header <- peekElemOff p 1
    if header == (0::Int32)
      then protoNode <$> peekElemOff p 0 <*> peekElemOff p 2 <*> peekElemOff p 3 <*> peekElemOff p 4 <*> peekElemOff p 5 <*> peekElemOff p 6
      else let !p = castPtr q in ParseDetails <$> peekElemOff p 0 <*> peekElemOff p 1 <*> peekElemOff p 2 <*> peekElemOff p 3 <*> peekElemOff p 4
     where
       protoNode no nl ao al = ProtoParseDetails (Slice no nl) (Slice ao al)

data AttributeParseDetails =
  AttributeParseDetails
  { nameA :: {-# UNPACK #-} !Slice,
    value :: {-# UNPACK #-} !Slice
  }
  deriving (Eq, Show)

instance Storable AttributeParseDetails where
  sizeOf _ = sizeOf empty * 2
  alignment _ = alignment (0 :: Int)
  peek !q = do
    let !p = castPtr q :: Ptr Slice
    !a <- peekElemOff p 0
    !b <- peekElemOff p 1
    return (AttributeParseDetails a b)
  poke !q (AttributeParseDetails a b)= do
    let !p = castPtr q :: Ptr Slice
    pokeElemOff p 0 a
    pokeElemOff p 1 b

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
