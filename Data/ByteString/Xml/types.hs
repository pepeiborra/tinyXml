{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DisambiguateRecordFields, DuplicateRecordFields, NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

data AttributeList =
    ASnoc !AttributeList {-# UNPACK #-} !Attribute
  | ANil
  deriving Show
data NodeContentList =
    CSnoc !NodeContentList !NodeContent
  | CNil
  deriving Show

data Node =
  Node
  { name       :: {-# UNPACK #-} !Str,
    start      :: {-# UNPACK #-} !Int,
    source     :: {-# UNPACK #-} !(ForeignPtr Word8),
    _attributes :: !AttributeList,
    contents   :: !NodeContentList
  }

instance Show Node where
  show Node{name,_attributes,contents} =
    show (name,_attributes,contents)

makeLenses ''Node

{- Magical Lens dust to dress things as lists -}

makePrisms ''AttributeList

instance Each AttributeList AttributeList Attribute Attribute where
  each f (ASnoc al a) = flip ASnoc <$> f a <*> each f al
  each _ ANil = pure ANil

instance Snoc AttributeList AttributeList Attribute Attribute where _Snoc = _ASnoc

instance AsEmpty AttributeList where _Empty = _ANil

makePrisms ''NodeContentList

instance Each NodeContentList NodeContentList NodeContent NodeContent where
  each _ CNil = pure CNil
  each f (CSnoc nl n) = flip CSnoc <$> f n <*> each f nl

instance Snoc NodeContentList NodeContentList NodeContent NodeContent where _Snoc = _CSnoc

instance AsEmpty NodeContentList where _Empty = _CNil

makePrisms ''NodeContent

instance Plated Node where
  plate f (Node n l s a nn) = Node n l s a <$> each (_NodeChild f) nn

{- Error types -}

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

