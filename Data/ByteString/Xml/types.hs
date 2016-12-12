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

data ElementData =
    ElementData
            { _name       :: {-# UNPACK #-} !Str
            , _attributes :: !AttributeList
            , _contents   :: !NodeList }
    deriving Show

data NodeType =
    Text {-# UNPACK #-} !Str
  | Element {-# UNPACK #-} !ElementData deriving Show

data AttributeList =
    ASnoc !AttributeList {-# UNPACK #-} !Attribute
  | ANil
  deriving Show

data NodeList =
    NSnoc !NodeList {-# UNPACK #-} !Node
  | NNil
  deriving Show

data Node =
  Node
  { _source     :: {-# UNPACK #-} !(ForeignPtr Word8),
    _details    :: !NodeType
  }

instance Show Node where
  show Node{_source, _details = Element ElementData{_name,_attributes,_contents} } =
    show ((_name,_source)^.from indexPtr, _attributes, _contents)
  show Node{_source, _details=Text txt} =
    show $ (txt,_source) ^. from indexPtr

makeLenses ''Node
makeLenses ''NodeType
makeLenses ''ElementData
makePrisms ''NodeType
makePrisms ''AttributeList
makePrisms ''NodeList

{- Magical Lens dust to dress things as lists -}

instance Each AttributeList AttributeList Attribute Attribute where
  each f (ASnoc al a) = flip ASnoc <$> f a <*> each f al
  each _ ANil = pure ANil

instance Snoc AttributeList AttributeList Attribute Attribute where _Snoc = _ASnoc

instance AsEmpty AttributeList where _Empty = _ANil

instance Each NodeList NodeList Node Node where
  each _ NNil = pure NNil
  each f (NSnoc nl n) = flip NSnoc <$> f n <*> each f nl

instance Snoc NodeList NodeList Node Node where _Snoc = _NSnoc

instance AsEmpty NodeList where _Empty = _NNil

makePrisms ''Node

instance Plated Node where
  plate f (Node s nt) = Node s <$> (_Element.contents) (each f) nt

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
  | BadTagForm !SrcLoc
  | UnfinishedComment !SrcLoc
  | Garbage !SrcLoc
   deriving Show

instance Show Error where
  show (Error etype cs) = show etype ++ prettyCallStack cs

