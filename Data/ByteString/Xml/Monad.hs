{-# OPTIONS_GHC -fobject-code #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

module Data.ByteString.Xml.Monad where
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.Array.Base
import Data.Array.ST
import Data.ByteString.Internal (ByteString(..))
import Data.STRef
import Data.Word
import Control.Lens hiding (use)
import Foreign.ForeignPtr       (ForeignPtr)

import Data.ByteString.Xml.Types
import Data.Coerce

type MonadParse m = (MonadReader (ForeignPtr Word8) m, MonadState ParseState m)

type ParseState = Str
cursor = simple

data Env s = Env
  { _ptr :: ForeignPtr Word8
  , _offsetAndLength :: STUArray s Int Int -- True for Length
  }

newtype ParseMonad s a =
  PM {unPM :: Env s -> ST s a }


{-# INLINE runPM #-}
runPM :: ByteString -> (forall s. ParseMonad s a) -> a
runPM (PS fptr o l) pm = runST $ do
  refO <- newListArray (0,1) [o,l]
  unPM pm (Env fptr refO)

{-# INLINE asBS #-}
asBS :: Iso' (ParseState, ForeignPtr Word8) ByteString
asBS = (from indexPtr)

-- | Use the target of a lens on the current state _and_ environment
{-# INLINE useE #-}
useE :: MonadParse m => Getting a (ParseState, ForeignPtr Word8) a -> m a
useE l = do
  s <- get
  r <- ask
  return $ view l (s,r)

{-# INLINE overE #-}
overE l f = do
  r <- ask
  let f' s = ((), fst $ over l f (s,r))
  state f'

stateE :: MonadParse m => Iso' (ParseState, ForeignPtr Word8) s -> (s -> (a,s)) -> m a
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

throw :: ErrorType -> m a
throw etype = error $ show etype

{-# INLINE loc #-}
loc :: MonadParse m => m SrcLoc
loc = do
  Str o _ <- use cursor
  return (SrcLoc o)

throwLoc :: MonadParse m => (SrcLoc -> ErrorType) -> m a
throwLoc e = loc >>= throw . e

instance Functor (ParseMonad s) where
  fmap f  = PM . fmap (fmap f) . unPM
instance Applicative (ParseMonad s) where
  pure x = PM $ \_ -> return x
  PM pmf <*> PM pmx = PM (liftA2 (<*>) pmf pmx)
instance Monad (ParseMonad s) where
  return = pure
  {-# INLINE (>>=) #-}
  PM m >>= k = PM $ \ps -> do
    !a <- m ps
    !res <- unPM (k a) ps
    return res

instance MonadState ParseState (ParseMonad s) where
  {-# INLINE state #-} -- version in transformers lacks inline pragma
  state f = do
    st <- get
    let ( !a, st') = f st
    put st'
    return a

  {-# INLINE get #-}
  get   = PM $ \(Env _ a) -> Str <$> unsafeRead a 0 <*> unsafeRead a 1
  {-# INLINE put #-}
  put (Str o l) = PM $ \(Env _ a) -> do
   unsafeWrite  a 0 o
   unsafeWrite  a 1 l

instance MonadReader (ForeignPtr Word8) (ParseMonad s) where
  {-# INLINE ask #-}
  ask = PM $ \(Env fptr _) -> return fptr

