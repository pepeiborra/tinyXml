{-# OPTIONS_GHC -fobject-code #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash  #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

module Data.ByteString.Xml.Monad where
import qualified Control.Exception as CE
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.ByteString.Internal (ByteString(..), c2w, w2c, memchr, memcmp)
import Data.Int
import Data.List
import qualified Data.Vector.Unboxed.Mutable as U
import qualified Data.Vector.Storable.Mutable as M
import Data.Word
import Control.Lens hiding (use)
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Array

import Data.VectorBuilder.Storable as VectorBuilder
import Data.ByteString.Xml.Types
import Data.ByteString.Xml.Internal.Types

import Config
import GHC.Stack (CallStack)

type MonadParse m = (MonadReader (ForeignPtr Word8) m, MonadState ParseState m)

type ParseState = Slice
cursor = simple

data Env s =
  Env { source :: ForeignPtr Word8
      , ptr :: Ptr Word8
      , attributeBuffer :: VectorBuilder s Attribute
      , nodeBuffer :: VectorBuilder s (Node)
      , slice :: U.MVector s Int32 -- the offset and length in a 2 element vector
      }

newtype ParseMonad s a = PM {unPM :: Env s -> ST s a }

liftST :: ST s a -> ParseMonad s a
liftST action = PM $ \env -> action

unsafeLiftIO :: IO a -> ParseMonad s a
unsafeLiftIO action = liftST (unsafeIOToST action)

{-# INLINE asBS #-}
asBS :: Iso' (ParseState, ForeignPtr Word8) ByteString
asBS = (from indexPtr)

runPM :: ByteString -> (forall s. ParseMonad s a) -> a
runPM (PS fptr o l) pm = runST $ do
  attributes <- VectorBuilder.new 1000
  nodes      <- VectorBuilder.new 50
  slice      <- U.new 2
  U.unsafeWrite slice 0 (fromIntegral o)
  U.unsafeWrite slice 1 (fromIntegral l)
  unsafeIOToST $ withForeignPtr fptr $ \ptr -> unsafeSTToIO $ unPM pm (Env fptr ptr attributes nodes slice)

readStr :: Slice -> ParseMonad s ByteString
readStr str = do
  Env{source} <- getEnv
  return $ (str,source) ^. from indexPtr

-- | Use the target of a lens on the current state _and_ environment
{-# INLINE useE #-}
useE :: MonadParse m => Getting a (ParseState, ForeignPtr Word8) a -> m a
useE l = do
  s <- get
  r <- ask
  return $ view l (s,r)

-- | Apply a lens over the current state _and_ environment
{-# INLINE overE #-}
overE l f = do
  r <- ask
  let f' s = ((), fst $ over l f (s,r))
  state f'

-- | Apply a state function to the state and environment, under the given iso
stateE :: MonadParse m => Iso' (ParseState, ForeignPtr Word8) s -> (s -> (a,s)) -> m a
{-# INLINE stateE #-}
stateE iso f = withIso iso $ \from to -> do
  r <- ask
  let f' s =
        let (a, sr') = f (from(s,r))
            (s',_r') = to sr'
        in  (a, s')
  state f'

{-# INLINE (%==) #-}
l %== f = overE l f

{-# INLINE (.==) #-}
l .== v = overE l (const v)

-- The original uses State.modify, which lacks an inline pragma
{-# INLINE (%=) #-}
l %= f = state (\s -> ((), s & l %~ f))

-- The original uses State.modify, which lacks an inline pragma
{-# INLINE (.=) #-}
l .= f = state (\s -> ((), s & l .~ f))

-- The original uses State.gets, which lacks an inline pragma
{-# INLINE use #-}
use l = fmap (view l) get

bsHead :: ParseMonad s Word8
bsHead = bsIndex 0

bsIndex :: Int -> ParseMonad s Word8
bsIndex i = do
  Env{ptr=p, slice} <- getEnv
  o <- U.unsafeRead slice 0
  unsafeLiftIO( peekByteOff p (i + fromIntegral o))

bsIsPrefix (PS r o' l') = do
  Env{ptr=p, slice} <- getEnv
  o <- U.unsafeRead slice 0
  !resp <-
        unsafeLiftIO $ withForeignPtr r $ \p' ->
          memcmp (p' `plusPtr` o') (p `plusPtr` fromIntegral o) (fromIntegral l')
  let !res = resp == 0
  return res

bsElemIndex c = do
  Env{ptr=p, slice} <- getEnv
  o <- U.unsafeRead slice 0
  l <- U.unsafeRead slice 1
  let !p' = p `plusPtr` fromIntegral o
  !q <- unsafeLiftIO $ do memchr p' (c2w c) (fromIntegral l)
  let !res = if q == nullPtr then Nothing else Just $! q `minusPtr` p'
  return res

bsDropWhile f = do
  Env{ptr=p, slice} <- getEnv
  o <- U.unsafeRead slice 0
  l <- U.unsafeRead slice 1
  !n <- unsafeLiftIO $ findIndexOrEnd (not.f.w2c) p (fromIntegral o) l
  U.unsafeWrite slice 0 (o+n)
  U.unsafeWrite slice 1 (l-n)
  return ()

bsSpan f = do
  Env{ptr=p, slice} <- getEnv
  o <- U.unsafeRead slice 0
  l <- U.unsafeRead slice 1
  !n <- unsafeLiftIO $ findIndexOrEnd (not.f.w2c) p (fromIntegral o) l
  U.unsafeWrite slice 0 (o+n)
  U.unsafeWrite slice 1 (l-n)
  return $! Slice o n

findIndexOrEnd k f o l = go (f `plusPtr` o) 0 where
    go !ptr !n | n >= l    = return l
               | otherwise = do w <- peek ptr
                                if k w
                                  then return n
                                  else go (ptr `plusPtr` 1) (n+1)
{-# INLINE findIndexOrEnd #-}

{-# INLINE loc #-}
loc :: MonadParse m => m SrcLoc
loc = do
  Slice o _ <- use cursor
  return (SrcLoc $ fromIntegral o)

throw :: HasCallStack => ErrorType -> a
#if __GLASGOW_HASKELL__ < 800
throw e = CE.throw $ Error e ?callStack
#else
throw e = CE.throw $ Error e callStack
#endif

throwLoc :: HasCallStack => MonadParse m => (SrcLoc -> ErrorType) -> m a
throwLoc e = loc >>= throw . e

type Builder s a = ParseMonad s (Maybe a)

getEnv = PM $ \env -> return env

-- | Exhausts the builder and returns the recorded slice
recordAttributes :: Config => Builder s Attribute -> ParseMonad s Slice
recordAttributes builder = do
  first <- builder
  case first of
    Nothing -> return sliceEmpty
    Just a  -> do
      Env{attributeBuffer} <- getEnv
      firstIx <- VectorBuilder.insert attributeBuffer a
      -- we assume that the VectorBuilder produces sequential indexes
      -- and that no one else is inserting concurrently
      let go n = do
            next <- builder
            case next of
              Nothing -> return n
              Just x -> do
                _ <- VectorBuilder.insert attributeBuffer x
                return (n+1)
      count <- go 0
      return (Slice (fromIntegral firstIx) count)

updateNode :: Config => Int -> (Ptr Node -> IO ()) -> ParseMonad s ()
updateNode i f = do
  Env{nodeBuffer} <- getEnv
  vector <- VectorBuilder.unsafeToMVector nodeBuffer
  let (fptr,_l) = M.unsafeToForeignPtr0 vector
  unsafeLiftIO $ withForeignPtr fptr $ \ptr -> f (ptr `advancePtr` i)
  return ()
{-# INLINE updateNode #-}

-- | Push a node into the temporary stack
pushNode :: Config => Node -> ParseMonad s ()
pushNode node = do
  Env{nodeBuffer} <- getEnv
  VectorBuilder.push nodeBuffer node
{-# INLINE pushNode #-}

-- | Pop the top node from the temporary stack into permanent storage
popNode :: Config => (Node -> Node) -> ParseMonad s Int
popNode f = do
  Env{nodeBuffer} <- getEnv
  VectorBuilder.pop f nodeBuffer
{-# INLINE popNode #-}

-- | Insert a node into permanent storage
insertNode :: Config => Node -> ParseMonad s Int
insertNode node = do
  Env{nodeBuffer} <- getEnv
  VectorBuilder.insert nodeBuffer node
{-# INLINE insertNode #-}

getNodeBufferCount, getNodeStackCount :: Config => ParseMonad s Int
getNodeBufferCount = do
  Env{nodeBuffer} <- getEnv
  VectorBuilder.getCount nodeBuffer
{-# INLINE getNodeBufferCount #-}

getNodeStackCount = do
  Env{nodeBuffer} <- getEnv
  VectorBuilder.getStackCount nodeBuffer
{-# INLINE getNodeStackCount #-}

instance Functor (ParseMonad s) where
  fmap f (PM m) = PM $ \e -> fmap f (m e)
instance Applicative (ParseMonad s) where
  pure x = PM $ \_ -> return x
  PM pmf <*> PM pmx = PM $ \e -> pmf e <*> pmx e

instance Monad (ParseMonad s) where
  return = pure
  {-# INLINE (>>=) #-}
  PM m >>= k = PM $ \e -> m e >>= \x -> unPM (k x) e

instance MonadState ParseState (ParseMonad s) where
  {-# INLINE state #-} -- version in transformers lacks inline pragma
  state f = PM $ \Env{slice} -> do
    o <- U.unsafeRead slice 0
    l <- U.unsafeRead slice 1
    let !(!a, Slice o' l') = f (Slice o l)
    U.unsafeWrite slice 0 o'
    U.unsafeWrite slice 1 l'
    return a

  {-# INLINE get #-}
  get = PM $ \Env{slice} -> do
    o <- U.unsafeRead slice 0
    l <- U.unsafeRead slice 1
    return $! Slice o l

  {-# INLINE put #-}
  put (Slice o' l') = PM $ \Env{slice} -> do
    U.unsafeWrite slice 0 o'
    U.unsafeWrite slice 1 l'
    return ()

instance MonadReader (ForeignPtr Word8) (ParseMonad s) where
  {-# INLINE ask #-}
  ask = PM $ \Env{source=fptr} -> return fptr

instance PrimMonad (ParseMonad s) where
  type PrimState(ParseMonad s) = s
  primitive m = PM $ \env -> primitive m

