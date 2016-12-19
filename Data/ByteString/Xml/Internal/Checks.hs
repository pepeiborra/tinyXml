{-# LANGUAGE ViewPatterns, CPP #-}
module Data.ByteString.Xml.Internal.Checks where

import Control.Monad
import Data.ByteString.Xml.Monad
import Data.ByteString.Xml.Types as Slice
import Data.ByteString.Xml.Internal.Types
import qualified Data.ByteString.Char8 as BS
import Text.Printf

#ifdef STACK_CONSISTENCY_CHECKS
doStackChecks = True
#else
doStackChecks = False
#endif

checkConsistency (fromIntegral -> outerStart) (fromIntegral -> innerClose) origStr seenStr
  | doStackChecks = do
            nameBS_original <- readStr origStr
            seenBS <- readStr seenStr
            unless (seenBS == nameBS_original) $ do
                xml <- readStr(Slice outerStart (min 100 $ innerClose + fromIntegral( Slice.length origStr) + 4 - outerStart))
                let go = do
                      c <- getNodeStackCount
                      if c > 0
                        then do
                          n <- popNode
                          nBS <- readStr (name n)
                          (BS.unpack nBS :) <$> go
                        else return []
                stackNames <- go
                error $ printf "Inconsistency detected in the node stack while parsing %s...\n Expected %s(%s), but obtained %s(%s) (and the stack contains %s)"
                                (BS.unpack xml) (BS.unpack nameBS_original) (show origStr) (BS.unpack seenBS) (show seenStr)(show stackNames)
  | otherwise =
      return ()

{-# INLINE checkConsistency #-}
