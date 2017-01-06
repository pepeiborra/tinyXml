{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances #-}
module Text.Xml.Tiny.Internal.Types where

import Text.Xml.Tiny.Types as Slice
import Foreign
import Foreign.C

data Attribute =
  Attribute
  { nameA :: {-# UNPACK #-} !Slice,
    value :: {-# UNPACK #-} !Slice
  }
  deriving (Eq, Show)

instance Storable Attribute where
  sizeOf _ = sizeOf sliceEmpty * 2
  alignment _ = alignment (0 :: CInt)
  peek !q = do
    let !p = castPtr q :: Ptr Slice
    !a <- peekElemOff p 0
    !b <- peekElemOff p 1
    return (Attribute a b)
  poke !q (Attribute a b)= do
    let !p = castPtr q :: Ptr Slice
    pokeElemOff p 0 a
    pokeElemOff p 1 b

-- A parsed node is defined by:
--  * name       (string slice)
--  * inner xml  (string slice)
--  * outer xml  (string slice)
--  * attributes (attr slice)
--  * contents   (Node slice)

data Node =
    Node { name, inner, outer, attributes, nodeContents :: {-# UNPACK #-} !Slice }
    -- An incompletely defined Node
  | ProtoNode { name, attributes :: !Slice, innerStart, outerStart :: !Int32 }
  deriving (Show)

-- | Assumes that a name can never be the empty slice
instance Storable Node where
  sizeOf    _ = sizeOf sliceEmpty * 5
  alignment _ = alignment(0::CInt)
  poke !q (Node a b c d e)  = let p = castPtr q in pokeElemOff p 0 a >> pokeElemOff p 1 b >> pokeElemOff p 2 c >> pokeElemOff p 3 d >> pokeElemOff p 4 e
  poke !q (ProtoNode (Slice no nl) (Slice ao al) i o) = do
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
      else let !p = castPtr q in Node <$> peekElemOff p 0 <*> peekElemOff p 1 <*> peekElemOff p 2 <*> peekElemOff p 3 <*> peekElemOff p 4
     where
       protoNode no nl ao al = ProtoNode (Slice no nl) (Slice ao al)

updateInner, updateOuter, updateContents :: Slice -> Ptr Node -> IO ()
updateInner newV ptr = pokeByteOff ptr (1 * sizeOf sliceEmpty) newV
updateOuter newV ptr = pokeByteOff ptr (2  *sizeOf sliceEmpty) newV
updateContents newV ptr = pokeByteOff ptr (4 * sizeOf sliceEmpty) newV

