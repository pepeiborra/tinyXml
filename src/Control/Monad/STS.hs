{-# LANGUAGE Unsafe #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables, PolyKinds, TypeInType #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples, RankNTypes #-}
{-# OPTIONS_GHC -fobject-code #-}

module Control.Monad.STS (
        STS(..), STSRep,
        runSTS,
        unsafeIOToSTS

        -- * Unsafe functions
    ) where

import GHC.Base
import GHC.Exts
import GHC.IO
import GHC.ST
import GHC.Show

import Control.Monad.State.Class
import Control.Monad.Primitive

default ()

newtype STS s a = STS {unSTS :: STSRep s a}
type STSRep s a = State# s -> Int# -> Int# -> (# State# s, Int#, Int#, a #)

liftST :: ST s a -> STS s a
liftST (ST m) = STS $ \s o l -> case m s of (# s', r #) -> (# s', o, l, r #)
unsafeIOToSTS :: IO a -> STS s a
unsafeIOToSTS = Control.Monad.STS.liftST . unsafeIOToST

-- | @since 2.01
instance Functor (STS s) where
    fmap f (STS m) = STS $ \ s o l ->
      case (m s o l) of { (# new_s, o', l', r #) ->
      (# new_s, o', l', f r #) }

-- | @since 4.4.0.0
instance Applicative (STS s) where
    {-# INLINE pure #-}
    pure x = STS (\ s o l -> (# s, o, l, x #) )
    (<*>) = ap

-- | @since 2.01
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

-- | @since 2.01
instance  Show (STS s a)  where
    showsPrec _ _  = showString "<<STS action>>"
    showList       = showList__ (showsPrec 0)

{-# INLINE runSTS #-}
runSTS :: Int# -> Int# -> (forall s. STS s a) -> a
runSTS o l (STS st_rep) =
  let comp s = case st_rep s o l of  (# s', o', l', r #) -> (# s', r #)
  in case runRW# comp of (# _, a #) -> a
-- See Note [Definition of runRW#] in GHC.Magic
