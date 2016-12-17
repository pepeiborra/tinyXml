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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import GHC.Stack (HasCallStack, CallStack, callStack)

type MonadParse m = (MonadReader (ForeignPtr Word8) m, MonadState ParseState m)

type ParseState = Slice
cursor = simple

data Env s =
  Env { source :: ForeignPtr Word8
      , ptr :: Ptr Word8
      , attributeBuffer :: VectorBuilder s Attribute
      , nodeBuffer :: VectorBuilder s (Node)
      }

newtype ParseMonad s a =
  PM {unPM :: Env s -> Int32 -> Int32 -> ST s ( a, Int32, Int32 ) }

liftST :: ST s a -> ParseMonad s a
liftST action = PM $ \env o l -> action <$$> (,o,l)

unsafeLiftIO :: IO a -> ParseMonad s a
unsafeLiftIO action = liftST (unsafeIOToST action)

{-# INLINE asBS #-}
asBS :: Iso' (ParseState, ForeignPtr Word8) ByteString
asBS = (from indexPtr)

runPM :: ByteString -> (forall s. ParseMonad s a) -> a
runPM (PS fptr o l) pm = runST $ do
  attributes <- VectorBuilder.new 1000
  nodes      <- VectorBuilder.new 50
  unsafeIOToST $ withForeignPtr fptr $ \ptr -> unsafeSTToIO $ do
    ( a, _, _ ) <- unPM pm (Env fptr ptr attributes nodes) (fromIntegral o) (fromIntegral l)
    return a

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
bsIndex i = PM $ \Env{ptr=p} o l -> unsafeIOToST( peekByteOff p (i + fromIntegral o)) <$$> (, o, l )

bsIsPrefix (PS r o' l') = PM$ \Env{ptr=p} o l -> do
   !resp <-
        unsafeIOToST $ withForeignPtr r $ \p' ->
          memcmp (p' `plusPtr` o') (p `plusPtr` fromIntegral o) (fromIntegral l')
   let !res = resp == 0
   return ( res, o, l )
bsElemIndex c = PM$ \Env{ptr=p} o l -> do
  let !p' = p `plusPtr` fromIntegral o
  !q <- unsafeIOToST $ do memchr p' (c2w c) (fromIntegral l)
  let !res = if q == nullPtr then Nothing else Just $! q `minusPtr` p'
  return ( res, o, l )

bsDropWhile f = PM$ \Env{ptr=p} o l -> do
  !n <- unsafeIOToST $ findIndexOrEnd (not.f.w2c) p (fromIntegral o) l
  return ( (), o + n, l - n )

bsSpan f = PM$ \Env{ptr=p} o l -> do
  !n <- unsafeIOToST $ findIndexOrEnd (not.f.w2c) p (fromIntegral o) l
  return ( Slice o n, o + n, l - n )

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
throw e = CE.throw $ Error e callStack

throwLoc :: HasCallStack => MonadParse m => (SrcLoc -> ErrorType) -> m a
throwLoc e = loc >>= throw . e

type Builder s a = ParseMonad s (Maybe a)

getEnv = PM $ \env o l -> return (env,o,l)

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
  fmap f (PM m) = PM $ \e o l -> do { ( a, o', l' ) <- m e o l ; return ( f a, o', l' ) }
instance Applicative (ParseMonad s) where
  pure x = PM $ \_ o l -> return ( x, o, l )
  PM pmf <*> PM pmx = PM $ \e o l -> do
    ( f, o',  l'  ) <- pmf e o l
    ( x, o'', l'' ) <- pmx e o' l'
    return ( f x, o'', l'' )

instance Monad (ParseMonad s) where
  return = pure
  {-# INLINE (>>=) #-}
  PM m >>= k = PM $ \e o l -> do
    (!a, !o', !l') <- m e o l
    let !next = k a in unPM next e o' l'

instance MonadState ParseState (ParseMonad s) where
  {-# INLINE state #-} -- version in transformers lacks inline pragma
  state f = PM $ \_ o l ->
    let !(!a, Slice o' l') = f (Slice o l)
    in return ( a, o', l' )

  {-# INLINE get #-}
  get = PM $ \ _ o l -> return ( Slice o l, o, l )
  {-# INLINE put #-}
  put (Slice o' l') = PM $ \_ _ _ -> return ( (), o', l' )

instance MonadReader (ForeignPtr Word8) (ParseMonad s) where
  {-# INLINE ask #-}
  ask = PM $ \Env{source=fptr} o l -> return ( fptr, o, l )

instance PrimMonad (ParseMonad s) where
  type PrimState(ParseMonad s) = s
  primitive m = PM $ \env o l -> primitive m <$$> (, o, l)

(<$$>) :: Functor f => f a -> (a->b) -> f b
(<$$>) = flip fmap

