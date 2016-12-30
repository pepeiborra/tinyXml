{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module Data.ByteString.Xml.Internal.Types where

import Data.ByteString.Xml.Types as Slice
import Data.ByteString.Internal (ByteString(..))
import Foreign
import Foreign.C

data Attribute =
  Attribute
  { nameA :: Slice,
    value :: Slice
  }
  deriving (Eq, Show)

instance Storable Attribute where
  sizeOf _ = sizeOf sliceEmpty * 2
  alignment _ = alignment (0 :: CInt)
  peek q = do
    let p = castPtr q :: Ptr Slice
    a <- peekElemOff p 0
    b <- peekElemOff p 1
    return (Attribute a b)
     where
       peekElemOff ptr off  = peekByteOff ptr (off * s)
       s = sizeOf sliceEmpty
  poke q (Attribute a b)= do
    let p = castPtr q :: Ptr Slice
    pokeElemOff p 0 a
    pokeElemOff p 1 b

-- A parsed node is defined by:
--  * name       (string slice)
--  * inner xml  (string slice)
--  * outer xml  (string slice)
--  * attributes (attr slice)
--  * contents   (Node slice)

data Node =
  Node { name, inner, outer, attributes, nodeContents :: Slice }
  deriving (Show)

instance Storable Node where
  sizeOf    _ = sizeOf sliceEmpty * 5
  alignment _ = alignment(0::CInt)
  peek q = let p = castPtr q in Node <$> peekElemOff p 0 <*> peekElemOff p 1 <*> peekElemOff p 2 <*> peekElemOff p 3 <*> peekElemOff p 4
  poke q (Node a b c d e) = let p = castPtr q in pokeElemOff p 0 a >> pokeElemOff p 1 b >> pokeElemOff p 2 c >> pokeElemOff p 3 d >> pokeElemOff p 4 e

updateInner, updateOuter, updateContents :: Slice -> Ptr Node -> IO ()
updateInner newV ptr = pokeByteOff ptr (1 * sizeOf sliceEmpty) newV
updateOuter newV ptr = pokeByteOff ptr (2  *sizeOf sliceEmpty) newV
updateContents newV ptr = pokeByteOff ptr (4 * sizeOf sliceEmpty) newV

