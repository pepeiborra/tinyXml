{-# LANGUAGE ViewPatterns, NamedFieldPuns, CPP #-}
module Text.Xml.Tiny.Internal.Checks where

import Text.Printf

#ifdef ENABLE_CONSISTENCY_CHECKS
doStackChecks = True
doCursorChecks = True
doParseTableChecks = True
#else
doStackChecks = False
doCursorChecks = False
doParseTableChecks = False
#endif
{-# INLINE doStackChecks #-}
{-# INLINE doCursorChecks #-}
{-# INLINE doParseTableChecks #-}

-- checkCursor :: ParseMonad s ()
checkBSaccess o l o0 l0 =
      let valid n = let x = fromIntegral n in x >= o0 && x < o0+l0
          check = (valid o && valid (o+l))
              || error (printf "access to source out of bounds: o=%d, l=%d" o l)
      in check `seq` ()
