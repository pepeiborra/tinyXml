{-# LANGUAGE TemplateHaskell, BangPatterns, DisambiguateRecordFields, NamedFieldPuns #-}
module Data.ByteString.Xml.Types where

import Control.Lens
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Internal (ByteString(..))
import Data.Sequence
import Data.Word
import GHC.Stack hiding (SrcLoc)
import Foreign.ForeignPtr       (ForeignPtr, withForeignPtr)

-- Alternative design - a shrunk version of ByteString relative to a ForeignPtr
data Str =
  Str { offset :: {-# UNPACK #-} !Int, length :: {-# UNPACK #-} !Int }
  deriving (Eq,Show)

strEmpty = Str 0 0
{-# INLINE null #-}
null (Str _ l) = l == 0
{-# INLINE take #-}
take i (Str o l) = Str o i -- unsafe
{-# INLINE drop #-}
drop i (Str o l) = Str (o+i) (l-i) -- unsafe

{-# INLINE indexPtr #-}
indexPtr :: Iso' ByteString (Str, ForeignPtr Word8)
indexPtr = iso fromBS toBS where
  fromBS (PS fptr o l) = (Str o l, fptr)
  toBS (Str o l, fptr) = PS fptr o l

data Attribute =
  Attribute
  { nameA :: {-# UNPACK #-}!Str,
    value :: {-# UNPACK #-}!Str
  }
  deriving (Eq, Show)

newtype Source = Source ByteString

data NodeContent =
  NodeText  {-# UNPACK #-} !Str |
  NodeChild {-# UNPACK #-} !Node
  deriving Show

data Node =
  Node
  { name       :: {-# UNPACK #-} !Str,
    start      :: {-# UNPACK #-} !Int,
    source     :: {-# UNPACK #-} !(ForeignPtr Word8),
    attributes :: !(Seq Attribute),
    contents   :: !(Seq NodeContent)
  }

instance Show Node where
  show Node{name,attributes,contents} =
    show (name,attributes,contents)

makePrisms ''NodeContent

instance Plated Node where
  plate f (Node n l s a nn) = Node n l s a <$> traverse (_NodeChild f) nn

newtype SrcLoc = SrcLoc Int deriving Show

data Error = Error ErrorType CallStack
data ErrorType =
    UnterminatedComment !SrcLoc
  | UnterminatedTag !Str !SrcLoc
  | ClosingTagMismatch !Str !SrcLoc
  | JunkAtTheEnd !Str !SrcLoc
  | UnexpectedEndOfStream
  | BadAttributeForm !SrcLoc
  | UnfinishedComment !SrcLoc
  | Garbage !SrcLoc
   deriving Show

instance Show Error where
  show (Error etype cs) = show etype ++ prettyCallStack cs

