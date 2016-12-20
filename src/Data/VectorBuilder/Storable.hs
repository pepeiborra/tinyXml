{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE CPP #-}
module Data.VectorBuilder.Storable
  (VectorBuilder, new, insert, push, pop, getCount, getStackCount, finalize, unsafeToMVector
  ) where

import Control.Exception (assert)
import Control.Monad
import Control.Monad.Primitive
import Data.Primitive.MutVar
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Generic.Mutable as M
--import qualified Data.Vector.Storable.Mutable as M
import qualified Data.Vector.Unboxed.Mutable  as U
import Foreign (Storable)
import Config
import Text.Printf

data VectorBuilder s a =
  VectorBuilder
    {
      -- We use an unboxed vector instead of a MutVar, which would have to be boxed
      --  (writing to MutVar is slow because of GC book keeping)
      next  :: {-# UNPACK #-} !(U.MVector s Int)  -- ^ The next free index in the mutable store
      -- The MVector is in a MutVar because grow is not in-place
    , store :: {-# UNPACK #-} !(MutVar s (S.MVector s a))    -- ^ The mutable store
    }

readU  :: (Config, _) => _
writeU :: (Config, _) => _
writeM :: (Config, _) => _
readM :: (Config, _) => _
copyM :: (Config, _) => _
sliceM :: (Config, _) => _
getCount :: (Config, _) => _

#ifdef ENABLE_VECTOR_CHECKS
readU  a = U.read  a
writeU a = U.write a
writeM a = M.write a
readM  a = M.read a
copyM  a = M.copy a
sliceM a = M.slice a
#else
readU  a = U.unsafeRead  a
writeU a = U.unsafeWrite a
writeM a = M.unsafeWrite a
readM  a = M.unsafeRead a
copyM  a = M.unsafeCopy a
sliceM a = M.unsafeSlice a
#endif

-- | Yields the number of elements stored in the buffer
getCount l = readU (next l) 0

-- | Yields the number of elements in the stack
getStackCount l = readU  (next l) 1

-- | Ensure that there is sufficient space
request :: (Config, Storable a, PrimMonad m) => Int -> VectorBuilder (PrimState m) a -> m ()
request n l = do
  frontCount <- readU  (next l) 0
  backCount  <- readU  (next l) 1
  assert (frontCount >= 0) $ return ()
  assert (backCount >= 0) $ return ()
  a <- readMutVar (store l)
  let len = M.length a
  unless (frontCount + backCount + n < len) $ do
        trace "VectorBuilder - grow" $ return ()
        a' <- M.basicUnsafeNew (len*2)
        copyM (sliceM 0 frontCount a') (sliceM 0 frontCount a)
        copyM (sliceM (2*len-backCount) backCount a') (sliceM (len-backCount) backCount a)
        writeMutVar (store l) a'
{-# INLINE request #-}

-- | Inserts an element at the next free position in the vector and returns the index
insert :: (Config, PrimMonad m, Storable a) => VectorBuilder (PrimState m) a -> a -> m Int
insert l v = do
  request 1 l
  nextI <- readU  (next l) 0
  writeU (next l) 0 (nextI + 1)
  a <- readMutVar (store l)
  writeM a nextI v
  return nextI
{-# INLINE insert #-}

-- | Push an element into a stack-like temporary storage.
push :: (Config, PrimMonad m, Storable a) => VectorBuilder (PrimState m) a -> a -> m ()
push l v = trace "VectorBuilder.push" $ do
  request 1 l
  stackCount <- readU  (next l) 1
  writeU (next l) 1 (stackCount + 1)
  assert (stackCount >= 0) $ return ()
  a <- readMutVar (store l)
  writeM a (M.length a - stackCount - 1) v
{-# INLINE push #-}

-- | Pop an element from the temporary stack into the next free position
--   Throws if the stack is empty!
pop :: (Config, PrimMonad m, Storable a) => VectorBuilder  (PrimState m) a -> m a
pop l = trace "VectorBuilder.pop" $ do
  backCount  <- readU (next l) 1 
  writeU (next l) 1 $! backCount-1
  a <- readMutVar (store l)
  let len = M.length a
  let from = len-backCount
  assert(backCount>0) $ return ()
  readM a from
{-# INLINE pop #-}

-- | Yield a short lived reference to the underlying foreign vector.
--   Note that Insert and Push may allocate a new underlying vector.
unsafeToMVector l = readMutVar (store l)

-- | Extracts an immutable vector. The VectorBuilder can no longer be used after this
finalize :: (Config, PrimMonad m, Storable a) => VectorBuilder (PrimState m) a -> m (S.Vector a)
finalize l = trace "VectorBuilder.finalize" $ do
  v <- readMutVar (store l)
  S.unsafeFreeze v

new :: (Config, PrimMonad m, Storable a) => Int -> m (VectorBuilder (PrimState m) a)
new initialSize = do
  next <- U.unsafeNew 2
  writeU next 0 0
  writeU next 1 0
  store <- M.unsafeNew initialSize
  storeRef <- newMutVar store
  return (VectorBuilder next storeRef)
