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

import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.ByteString.Internal (ByteString(..))
import Data.Word
import Control.Lens hiding (use)
import Foreign.ForeignPtr       (ForeignPtr)

import Data.ByteString.Xml.Types
import Data.Coerce

type MonadParse m = (MonadReader (ForeignPtr Word8) m, MonadState ParseState m)

type ParseState = Str
cursor = simple

newtype ParseMonad a = PM {runPM :: ForeignPtr Word8 -> ParseState -> (# a, ParseState #) }

{-# INLINE asBS #-}
asBS :: Lens' (ParseState, ForeignPtr Word8) ByteString
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
  s <- get
  r <- ask
  put (fst $ over l f (s,r))

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

instance Functor ParseMonad where fmap f (PM m) = PM $ \s ps -> case m s ps of (# a, ps' #) -> (# f a, ps' #)
instance Applicative ParseMonad where
  pure x = PM $ \_ ps -> (# x, ps #)
  PM pmf <*> PM pmx = PM $ \s ps ->
    let  (# f, ps'  #) = pmf s ps
         (# x, ps'' #) = pmx s ps'
    in (# f x, ps'' #)
instance Monad ParseMonad where
  return = pure
  {-# INLINE (>>=) #-}
  PM m >>= k = PM $ \s ps ->
    case m s ps of
      (# a, ps' #) -> runPM (k a) s ps'
instance MonadState ParseState ParseMonad where
  state f = PM $ \_ ps -> let ( !a, ps') = f ps in (# a, ps' #)
  get   = PM $ \_ ps -> (# ps, ps #)
  put x = PM $ \_ _  -> (# (), x #)

instance MonadReader (ForeignPtr Word8) ParseMonad where
  ask = PM $ \s ps -> (# s, ps #)

