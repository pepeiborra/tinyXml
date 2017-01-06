{-# LANGUAGE ConstraintKinds, KindSignatures, ImplicitParams, CPP #-}
module Config where
import GHC.Exts

import qualified GHC.Stack

import qualified Debug.Trace

trace :: String -> a -> a
#ifdef TRACE
trace msg = Debug.Trace.trace msg
#else
trace msg x = x
#endif
{-# INLINE trace #-}

#if __GLASGOW_HASKELL__ < 800
type HasCallStack = (?callStack :: GHC.Stack.CallStack)
#else
type HasCallStack = GHC.Stack.HasCallStack
#endif

#ifdef STACKTRACES
type Config = HasCallStack
#else
type Config = (() :: Constraint)
#endif

