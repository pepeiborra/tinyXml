{-# LANGUAGE ConstraintKinds, KindSignatures, ImplicitParams, CPP #-}
module Config where
import GHC.Exts

import qualified GHC.Stack

import qualified Debug.Trace

trace :: String -> a -> a
-- trace msg = Debug.Trace.trace msg
trace msg x = x
{-# INLINE trace #-}

#if __GLASGOW_HASKELL__ < 800
type HasCallStack = (?callStack :: GHC.Stack.CallStack)
#else
type HasCallStack = GHC.Stack.HasCallStack
#endif

#ifdef DEBUG
type Config = HasCallStack
#else
type Config = (() :: Constraint)
#endif

