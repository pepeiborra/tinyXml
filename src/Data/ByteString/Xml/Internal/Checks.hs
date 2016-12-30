{-# LANGUAGE ViewPatterns, NamedFieldPuns, CPP #-}
module Data.ByteString.Xml.Internal.Checks where

import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.ByteString.Internal
import Data.ByteString.Xml.Types as Slice
import Data.ByteString.Xml.Internal.Types
import qualified Data.ByteString.Char8 as BS
import Text.Printf

#ifdef ENABLE_CONSISTENCY_CHECKS
doStackChecks = True
doCursorChecks = True
#else
doStackChecks = False
doCursorChecks = False
#endif
{-# INLINE doStackChecks #-}
{-# INLINE doCursorChecks #-}

-- checkCursor :: ParseMonad s ()
checkBSaccess o l o0 l0 =
      let valid n = let x = fromIntegral n in x >= o0 && x < o0+l0
          _ = (valid o && valid (o+l))
              || error (printf "access to source out of bounds: o=%d, l=%d" o l)
      in ()
