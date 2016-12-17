{-# LANGUAGE ConstraintKinds, KindSignatures #-}
module Config where
import GHC.Exts

import GHC.Stack (HasCallStack)

import qualified Debug.Trace

trace :: String -> a -> a
-- trace msg = Debug.Trace.trace msg
trace msg x = x
{-# INLINE trace #-}

type Config = (() :: Constraint)

-- type Config = HasCallStack

