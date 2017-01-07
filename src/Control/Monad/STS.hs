{-# LANGUAGE Unsafe #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash, UnboxedTuples, RankNTypes #-}
{-# OPTIONS_GHC -fobject-code #-}
{-
Module: Control.Monad.STS
Description: A hand-rolled version of the ST monad with functional, unboxed state
Copyright: (c) Jose Iborra 2017
License: GPL-3
Maintainer: pepeiborra@gmail.com
Stability: experimental
-}
module Control.Monad.STS (
        STS(..), STSRep,
        runSTS,
        unsafeIOToSTS
    ) where

import GHC.Base
import GHC.IO
import GHC.ST
import GHC.Show

import Control.Monad.Primitive

-- | A hand-rolled State transformation of the ST monad
-- >   type STS s a = Slice -> ST (a, Slice)
newtype STS s a = STS {unSTS :: STSRep s a}

-- | The ST monad representation is an unboxed 2-tuple, but the second component has lifted kind
--   The naive transformation would yield
-- > State# -> Slice -> (# State#, (Slice, a))
--   So on every computation step we need to destruct and reconstruct the tuple (and the Slice, if its components are accessed)
--   In practice the optimizer is able to almost eliminate all the overhead, but a tiny bit remains.
--   So in order to avoid it, we roll our own monad by unpacking the slice contents into an unboxed 4-tuple.
type STSRep s a = State# s -> Int# -> Int# -> (# State# s, Int#, Int#, a #)

liftST :: ST s a -> STS s a
liftST (ST m) = STS $ \s o l -> case m s of (# s', r #) -> (# s', o, l, r #)
unsafeIOToSTS :: IO a -> STS s a
unsafeIOToSTS = Control.Monad.STS.liftST . unsafeIOToST

instance Functor (STS s) where
    fmap f (STS m) = STS $ \ s o l ->
      case m s o l of { (# new_s, o', l', r #) ->
      (# new_s, o', l', f r #) }

instance Applicative (STS s) where
    {-# INLINE pure #-}
    pure x = STS (\ s o l -> (# s, o, l, x #) )
    (<*>) = ap

instance Monad (STS s) where
    {-# INLINE (>>=)  #-}
    STS m >>= k
      = STS (\ s o l ->
        case m s o l of { (# new_s, o', l', r #) ->
        case k r of { STS k2 ->
        k2 new_s o' l' }})

instance PrimMonad (STS s) where
  type PrimState(STS s) = s
  {-# INLINE primitive #-}
  primitive m = STS $ \s o l -> let (# s', r #) = m s in (# s', o, l, r #)

instance  Show (STS s a)  where
    showsPrec _ _  = showString "<<STS action>>"
    showList       = showList__ shows

runSTS :: Int# -> Int# -> (forall s. STS s a) -> a
runSTS o l (STS st_rep) =
#if __GLASGOW_HASKELL__ >= 800
  let comp s = case st_rep s o l of  (# s', _o', _l', r #) -> (# s', r #)
  in case runRW# comp of (# _, a #) -> a
{-# INLINE runSTS #-}
#else
  case st_rep realWorld# o l of (# _, _, _, a #) -> a
{-# NOINLINE runSTS #-}
#endif
