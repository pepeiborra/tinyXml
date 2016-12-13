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

module Data.ByteString.Xml.Monad where
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.Array.Base
import Data.Array.ST
import Data.ByteString.Internal (ByteString(..), accursedUnutterablePerformIO, c2w, w2c, memchr, memcmp)
import Data.STRef
import Data.Word
import Control.Lens hiding (use)
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Ptr

import Data.ByteString.Xml.Types
import Data.Coerce
import GHC.Exts

type MonadParse m = (MonadReader (ForeignPtr Word8) m, MonadState ParseState m)

type ParseState = Str
cursor = simple

newtype ParseMonad a =
  PM {unPM :: ForeignPtr Word8 -> Ptr Word8 -> Int# -> Int# -> (# a, Int#, Int# #) }


{-# INLINE asBS #-}
asBS :: Iso' (ParseState, ForeignPtr Word8) ByteString
asBS = (from indexPtr)
runPM :: ByteString -> ParseMonad a -> a
runPM (PS fptr (I# o) (I# l)) pm = accursedUnutterablePerformIO $ withForeignPtr fptr $ \ptr -> let (# a, _, _ #) = unPM pm fptr ptr o l in return a

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

bsHead :: ParseMonad Word8
bsHead = PM $ \_r p o l -> (# accursedUnutterablePerformIO( peekByteOff p (I# o)), o, l #)

bsIndex :: Int -> ParseMonad Word8
bsIndex (I# i) = PM $ \_r p o l -> (# accursedUnutterablePerformIO( peekByteOff p (I#(i +# o))), o, l #)

bsIsPrefix (PS r o' l') = PM$ \_ p o l ->
   let !resp =
        accursedUnutterablePerformIO $ withForeignPtr r $ \p' ->
          memcmp (p' `plusPtr` o') (p `plusPtr` I# o) (fromIntegral l')
       !res = resp == 0
   in (# res, o, l #)
bsElemIndex c = PM$ \_ p o l ->
  let res = accursedUnutterablePerformIO $ do
        let !p' = p `plusPtr` I# o
        !q <- memchr p' (c2w c) (fromIntegral $ I# l)
        return $! if q == nullPtr then Nothing else Just $! q `minusPtr` p'
  in (# res, o, l #)

bsDropWhile f = PM$ \_ p o l ->
  let !i_n@(I# n) = accursedUnutterablePerformIO $ findIndexOrEnd (not.f.w2c) p (I# o) (I# l)
  in (# (), o +# n, l -# n #)

bsSpan f = PM$ \_ p o l ->
  let !i_n@(I# n) = accursedUnutterablePerformIO $ findIndexOrEnd (not.f.w2c) p (I# o) (I# l)
  in (# Str (I# o) i_n, o +# n, l -# n #)

findIndexOrEnd k f o l = go (f `plusPtr` o) 0 where
    go !ptr !n | n >= l    = return l
               | otherwise = do w <- peek ptr
                                if k w
                                  then return n
                                  else go (ptr `plusPtr` 1) (n+1)
{-# INLINE findIndexOrEnd #-}

throw :: ErrorType -> m a
throw etype = error $ show etype

{-# INLINE loc #-}
loc :: MonadParse m => m SrcLoc
loc = do
  Str o _ <- use cursor
  return (SrcLoc o)

throwLoc :: MonadParse m => (SrcLoc -> ErrorType) -> m a
throwLoc e = loc >>= throw . e

instance Functor (ParseMonad) where
  fmap f (PM m) = PM $ \r p o l -> let (# a, o', l' #) = m r p o l in (# f a, o', l' #)
instance Applicative (ParseMonad) where
  pure x = PM $ \_ _ o l -> (# x, o, l #)
  PM pmf <*> PM pmx = PM $ \s p o l ->
    let  (# f, o',  l'  #) = pmf s p o l
         (# x, o'', l'' #) = pmx s p o' l'
    in (# f x, o'', l'' #)

instance Monad (ParseMonad) where
  return = pure
  {-# INLINE (>>=) #-}
  PM m >>= k = PM $ \s p o l ->
    case m s p o l of
      (# a, o', l' #) -> unPM (k a) s p o' l'

instance MonadState ParseState (ParseMonad) where
  {-# INLINE state #-} -- version in transformers lacks inline pragma
  state f = PM $ \_ _ o l ->
    let !(!a, Str (I# o') (I# l')) = f (Str (I# o) (I# l))
    in (# a, o', l' #)
  {-# INLINE get #-}
  get = PM $ \ _ _ o l -> (# Str (I# o) (I# l), o, l #)
  {-# INLINE put #-}
  put (Str (I# o') (I# l')) = PM $ \_ _ _ _ -> (# (), o', l' #)

instance MonadReader (ForeignPtr Word8) (ParseMonad) where
  {-# INLINE ask #-}
  ask = PM $ \fptr _ o l -> (# fptr, o, l #)

