{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}
module Data.VectorBuilder.Storable
  (VectorBuilder, new, indexFromEnd, insert, push, pop, updateStackElt, getCount, getStackCount, finalize, unsafeToMVector, peek
  ) where

import Control.Exception (assert)
import Control.Monad
import Control.Monad.Primitive
import Data.Int
import Data.Primitive.MutVar
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import qualified Data.Vector.Generic.Mutable as M
--import qualified Data.Vector.Storable.Mutable as M
import qualified Data.Vector.Unboxed.Mutable  as U
import Foreign (Storable(sizeOf), Ptr, plusPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Config
import Text.Printf

default (Int, Double)

data VectorBuilder s a =
  VectorBuilder
    {
      -- We use an unboxed vector instead of a MutVar, which would have to be boxed
      --  (writing to MutVar is slow because of GC book keeping)
      next  :: {-# UNPACK #-} !(U.MVector s Int32)  -- ^ The next free index in the mutable store
      -- The MVector is in a MutVar because grow is not in-place
    , store :: {-# UNPACK #-} !(MutVar s (S.MVector s a))    -- ^ The mutable store
    }

readU  :: (Config, _) => _
writeU :: (Config, _) => _
writeM :: (Config, _) => _
readM :: (Config, _) => _
copyM :: (Config, _) => _
sliceM :: (Config, _) => _
getCount, getStackCount :: Config => PrimMonad m => VectorBuilder(PrimState m) a -> m Int32

#ifdef ENABLE_VECTOR_CHECKS
readU  a = U.read  a
writeU a = U.write a
writeM a ix = M.write a (fromIntegral ix)
readM  a ix = M.read a  (fromIntegral ix)
copyM  a = M.copy a
sliceM a b = M.slice (fromIntegral a) (fromIntegral b)
#else
readU  a = U.unsafeRead  a
writeU a = U.unsafeWrite a
writeM a !ix = M.unsafeWrite a (fromIntegral ix)
readM  a !ix = M.unsafeRead a (fromIntegral ix)
copyM  a = M.unsafeCopy a
sliceM a b = M.unsafeSlice (fromIntegral a) (fromIntegral b)
#endif

-- | Yields the number of elements stored in the buffer
getCount l = readU (next l) 0

-- | Yields the number of elements in the stack
getStackCount l = readU  (next l) 1

-- | Ensure that there is sufficient space
request :: (Config, Storable a, PrimMonad m) => Int32 -> VectorBuilder (PrimState m) a -> m ()
request !n VectorBuilder{next,store} = do
  frontCount <- readU  (next) 0
  backCount  <- readU  (next) 1
  assert (frontCount >= 0) $ return ()
  assert (backCount >= 0) $ return ()
  a <- readMutVar (store)
  let !len = M.length a
  unless (frontCount + backCount + n < fromIntegral len) $ do
        return ()
        a' <- M.basicUnsafeNew (len*2)
        copyM (sliceM 0 frontCount a') (sliceM 0 frontCount a)
        copyM (sliceM (2 * fromIntegral len - backCount) backCount a') (sliceM (fromIntegral len - backCount) backCount a)
        writeMutVar (store ) a'
{-# INLINE request #-}

-- | Inserts an element at the next free position in the vector and returns the index
insert :: (Config, PrimMonad m, Storable a) => VectorBuilder (PrimState m) a -> a -> m ()
insert l@VectorBuilder{next,store} v = do
  request 1 l
  nextI <- readU  (next ) 0
  writeU (next ) 0 (nextI + 1)
  a <- readMutVar (store )
  writeM a nextI v
{-# INLINE insert #-}

-- | Push an element into a stack-like temporary storage. Returns the index from the back
push :: (Config, PrimMonad m, Storable a) => VectorBuilder (PrimState m) a -> a -> m Int32
push l@VectorBuilder{next,store} !v = do
  request 1 l
  stackCount <- readU  (next) 1
  writeU (next) 1 (stackCount + 1)
  assert (stackCount >= 0) $ return ()
  a <- readMutVar (store)
  let ix = (fromIntegral $ M.length a) - stackCount - 1
  writeM a ix v
  return stackCount
{-# INLINE push #-}

-- | Peek into the temporary stack
peek :: Config => (Storable a, PrimMonad m, Storable a, Num n, Integral n, Ord n) => VectorBuilder (PrimState m) a -> n -> m a
peek VectorBuilder{next, store} n = do
  backCount <- readU next 1
  assert(fromIntegral backCount > n) $ return ()
  a <- readMutVar store
  readM a (M.length a - fromIntegral backCount - 1 + fromIntegral n)

updateStackElt :: Config => (Storable a, PrimMonad m, Storable a) => VectorBuilder (PrimState m) a -> Int32 -> (a -> a) -> m ()
updateStackElt VectorBuilder{store} i f = do
  arr <- readMutVar store
  let ix = (M.length arr - fromIntegral i - 1)
  a <- readM arr ix
  writeM arr ix (f a)
{-# INLINE updateStackElt #-}

indexFromEnd :: forall a m . Config => (Storable a, PrimMonad m, Storable a) => VectorBuilder (PrimState m) a -> Int32 -> m (Ptr a)
indexFromEnd VectorBuilder{store} i = do
  a <- readMutVar store
  let (fptr, _) = SM.unsafeToForeignPtr0 a
  let ptr = unsafeForeignPtrToPtr fptr
  let s = sizeOf(undefined :: a)
  return $ plusPtr ptr ((M.length a - fromIntegral i -  1) * s)

-- | Yield a short lived reference to the underlying foreign vector.
--   Note that Insert and Push may allocate a new underlying vector.
unsafeToMVector l = readMutVar (store l)

-- | Pop elements out of the temporary stack and append into the main buffer
pop :: Config => (Storable a, PrimMonad m, Storable a) => VectorBuilder (PrimState m) a -> Int32 -> m ()
pop l@VectorBuilder{next, store} n = do
  request n l
  backCount <- readU next 1
  frontCount <- readU next 0
  assert(backCount >= n) $ return ()
  writeU next 0 (frontCount +  n)
  writeU next 1 (backCount - n)
  a <- readMutVar store
  let !l = M.length a
  let !first = fromIntegral l - backCount
  forM_ [1..n] $ \i ->
    let from = frontCount - 1 + i
        to   = first+n-i
    in when (from /= to) $
        copyM (sliceM from 1 a) (sliceM to 1 a)
{-# INLINE pop #-}

-- | Extracts an immutable vector. The VectorBuilder can no longer be used after this
finalize :: (Config, PrimMonad m, Storable a) => VectorBuilder (PrimState m) a -> m (S.Vector a)
finalize l = trace "VectorBuilder.finalize" $ do
  v <- readMutVar (store l)
  frontCount <- readU (next l) 0
  S.unsafeFreeze (sliceM 0 frontCount v)

new :: (Config, PrimMonad m, Storable a) => Int -> m (VectorBuilder (PrimState m) a)
new initialSize = do
  next <- U.unsafeNew 2
  writeU next 0 0
  writeU next 1 0
  store <- M.unsafeNew initialSize
  storeRef <- newMutVar store
  return (VectorBuilder next storeRef)
