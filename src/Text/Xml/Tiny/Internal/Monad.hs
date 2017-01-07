{-# OPTIONS_GHC -fno-warn-name-shadowing -fobject-code #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ViewPatterns, BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash  #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE CPP #-}

module Text.Xml.Tiny.Internal.Monad where
import qualified Control.Exception as CE
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.STS
import Control.Monad.State.Class
import Data.ByteString.Internal (ByteString(..), c2w, w2c, memchr, memcmp)
import qualified Data.ByteString.Char8 as BS
import Data.Int
import Data.Word
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Storable hiding (peek)
import qualified Foreign.Storable as Foreign
import Foreign.Ptr
import GHC.Int

import Data.VectorBuilder.Storable (VectorBuilder)
import qualified Data.VectorBuilder.Storable as VectorBuilder
import Text.Xml.Tiny.Internal as Slice
import Text.Xml.Tiny.Internal.Checks

import Config
import qualified GHC.Stack
import Text.Printf

type MonadParse m = MonadState ParseState m

type ParseState = Slice

-- | The mutable environment of the ParseMonad.
data Env s =
  Env { source          :: !ByteString  -- ^ The input bytestring containing the XML document
      , ptr             :: {-# UNPACK #-} !(Ptr Word8) -- ^ A pointer to the raw bits of the above bytestring
      , attributeBuffer :: {-# UNPACK #-} !(VectorBuilder s AttributeParseDetails) -- ^ All the attributes parsed so far
      , nodeBuffer      :: {-# UNPACK #-} !(VectorBuilder s ParseDetails) -- ^ All the nodes parsed so far
      }

-- | A Reader Env, State Slice, ST monad,
--   the cursor Slice is deconstructed to its component in the STS monad.
newtype ParseMonad s a = PM {unPM :: Env s -> STS s a}

liftSTS :: Config => STS s a -> ParseMonad s a
liftSTS action = PM $ const action

unsafeLiftIO :: Config => IO a -> ParseMonad s a
unsafeLiftIO action = liftSTS (unsafeIOToSTS action)

runPM :: Config => ByteString -> (forall s. ParseMonad s a) -> a
runPM bs@(PS fptr (fromIntegral -> I32# o) (fromIntegral -> I32# l)) pm = runSTS o l $ do
  -- initial buffer sizes copied from hexml
  attributes <- VectorBuilder.new 1000
  nodes <- VectorBuilder.new 500
  let ptr = unsafeForeignPtrToPtr fptr
  unPM pm (Env bs ptr attributes nodes)

-- | Render a BS slice in the source bytestring.
readStr :: Config => Slice -> ParseMonad s ByteString
readStr str = do
  Env{source = PS fptr _ _} <- getEnv
  return $! fromIndexPtr str fptr

{-# INLINE getBS #-}
-- | Get the rest of the document from the current cursor
getBS :: Config => ParseMonad s ByteString
getBS = get >>= readStr

unsafeIO :: Config => String -> IO a -> ParseMonad s a
unsafeIO msg action = unsafeLiftIO $ do
#ifdef TRACE_UNSAFE
  res <- trace ("About to start unsafe IO: " ++ msg) $ action
  trace ("Completed unsafe IO: " ++ msg) $ return res
#else
  action
#endif

{-# INLINE withCursor #-}
withCursor k = do
  checkCursor
  Env{ptr=p} <- getEnv
  Slice o l <- get
  k p o l

-- | Look ahead. Peek at the current cursor (CPS to avoid unnecesary allocations)
peek :: Config => (Char -> a) -> ParseMonad s a
peek = peekAt 0

-- | Arbitrary look-ahead (CPS)
peekAt :: Config => Int32 -> (Char -> a) -> ParseMonad s a
peekAt i pred = withCursor $ \p o l ->
  if (l < i) then throw UnexpectedEndOfStream else peekAtUnsafePO i pred p o

{-# INLINE peekAtUnsafePO #-}
peekAtUnsafePO i pred p o = (pred.w2c) <$> unsafeIO "BS" (peekByteOff p (fromIntegral $ i + o))

-- | Look-ahead a pair of positions, optimizing a common case (CPS)
bsIndex2 :: Config => Int32 -> Int32 -> (Char -> Char -> a) -> ParseMonad s a
bsIndex2 i j pred = withCursor $ \p o l ->
  if (l<i || l<j) then throw UnexpectedEndOfStream else do
    checkCursor
    byteI <- peekAtUnsafePO i id p o
    byteJ <- peekAtUnsafePO j id p o
    return $ pred byteI byteJ

-- | isPrefix at the current cursor
bsIsPrefix :: Config => ByteString -> ParseMonad s Bool
bsIsPrefix (PS r o' l') = withCursor $ \p o _ -> do
  resp <-
        unsafeIO "memcmp" $ withForeignPtr r $ \p' ->
          memcmp (p' `plusPtr` o') (p `plusPtr` fromIntegral o) (fromIntegral l')
  let res = resp == 0
  return res

-- | elem index from the current cursor
bsElemIndex :: Char -> ParseMonad s (Maybe Int)
bsElemIndex c = withCursor $ \p o l -> do
  let !p' = p `plusPtr` fromIntegral o
  !q <- unsafeIO "memchr" $ memchr p' (c2w c) (fromIntegral l)
  let !res = if q == nullPtr then Nothing else Just $! q `minusPtr` p'
  return res
{-# INLINE bsElemIndex #-}

-- | drop while from the current cursor
bsDropWhile :: (Char -> Bool) -> ParseMonad s ()
bsDropWhile f = withCursor $ \p o l -> do
  !n <- unsafeIO "BS" $ findIndexOrEnd (not.f.w2c) (p `plusPtr` fromIntegral o) l
  put $ Slice (o+n) (l-n)
{-# INLINE bsDropWhile #-}

-- | Move the cursor ahead while the predicate is true, and return the dropped slice.
{-# INLINE bsSpan #-}
bsSpan :: (Char -> Bool) -> ParseMonad s Slice
bsSpan f = withCursor $ \ p o l -> do
  !n <- unsafeIO "BS" $ findIndexOrEnd (not.f.w2c) (p `plusPtr` fromIntegral o) l
  put $ Slice (o+n) (l-n)
  return $! Slice o n

{-# INLINE findIndexOrEnd #-}
findIndexOrEnd k p l = go p 0 where
    go !ptr !n | n >= l    = return l
               | otherwise = do !w <- Foreign.peek ptr
                                if k w
                                  then return n
                                  else go (ptr `plusPtr` 1) (n+1)

-- | Return the current cursor location
{-# INLINE loc #-}
loc :: ParseMonad s SrcLoc
loc = get >>= \ (Slice o _) -> return $ SrcLoc $ fromIntegral o

throw :: HasCallStack => ErrorType -> a
#if __GLASGOW_HASKELL__ < 800
throw e = CE.throw $ Error e ?callStack
#else
throw e = CE.throw $ Error e GHC.Stack.callStack
#endif

-- | Throw with the current cursor location
throwLoc :: Config => (SrcLoc -> ErrorType) -> ParseMonad s a
throwLoc e = loc >>= throw . e

{-# INLINE getEnv #-}
getEnv :: ParseMonad s (Env s)
getEnv = PM return

-- | Push a node into the temporary stack
pushNode :: Config => ParseDetails -> ParseMonad s Int32
pushNode node = do
  Env{nodeBuffer} <- getEnv
  VectorBuilder.push nodeBuffer node
{-# INLINE pushNode #-}

{-# INLINE popNodes #-}
-- | Pop the topmost n elements from the node buffer stack
popNodes :: Config => Int32 -> ParseMonad s ()
popNodes n = do
  Env{nodeBuffer} <- getEnv
  VectorBuilder.pop nodeBuffer n

peekNode :: Config => Int32 -> ParseMonad s ParseDetails
peekNode n = do
  Env{nodeBuffer} <- getEnv
  VectorBuilder.peek nodeBuffer n
{-# INLINE peekNode #-}

-- | Insert a node into permanent storage
insertNode :: Config => ParseDetails -> ParseMonad s ()
insertNode node = do
  Env{nodeBuffer} <- getEnv
  VectorBuilder.insert nodeBuffer node
{-# INLINE insertNode #-}

insertAttribute :: AttributeParseDetails -> ParseMonad s ()
insertAttribute att = do
  Env{attributeBuffer} <- getEnv
  VectorBuilder.insert attributeBuffer att
{-# INLINE insertAttribute #-}

getNodeBufferCount, getAttributeBufferCount, getNodeStackCount :: Config => ParseMonad s Int32
getNodeBufferCount = do
  Env{nodeBuffer} <- getEnv
  VectorBuilder.getCount nodeBuffer
{-# INLINE getNodeBufferCount #-}

getAttributeBufferCount = do
  Env{attributeBuffer} <- getEnv
  VectorBuilder.getCount attributeBuffer
{-# INLINE getAttributeBufferCount #-}

getNodeStackCount = do
  Env{nodeBuffer} <- getEnv
  VectorBuilder.getStackCount nodeBuffer
{-# INLINE getNodeStackCount #-}

-- | Update a node in the stack buffer
{-# INLINE updateNode #-}
updateNode :: Int32 -> (ParseDetails -> ParseDetails) -> ParseMonad s ()
updateNode i f = do
  Env{nodeBuffer} <- getEnv
  VectorBuilder.updateStackElt nodeBuffer i f

instance Functor (ParseMonad s) where
  fmap = liftM

instance Applicative (ParseMonad s) where
  pure x = PM (\_ -> return x)
  (<*>) = ap

instance Monad (ParseMonad s) where
  {-# INLINE (>>=) #-}
  PM m >>= k = PM (\e -> m e >>= \a -> unPM (k a) e)

instance MonadState ParseState (ParseMonad s) where
  get = PM $ \_ -> STS $ \ s o l -> (# s, o, l, Slice (I32# o) (I32# l) #)
  put (Slice (I32# o) (I32# l)) = PM $ \_ -> STS $ \ s _ _ ->
      (# s, o, l, () #)

instance PrimMonad (ParseMonad s) where
  type PrimState(ParseMonad s) = s
  {-# INLINE primitive #-}
  primitive m = PM $ \ _ -> primitive m

checkCursor
  | doCursorChecks = do
      Env{source = PS _ o0 l0} <- getEnv
      Slice o l <- get
      return $ checkBSaccess o l o0 l0
  | otherwise = return ()

checkConsistency (fromIntegral -> outerStart) (fromIntegral -> innerClose) origStr seenStr
  | doStackChecks = do
            nameBS_original <- readStr origStr
            seenBS <- readStr seenStr
            unless (seenBS == nameBS_original) $ do
                xml <- readStr(Slice outerStart (min 100 $ innerClose + fromIntegral( Slice.length origStr) + 4 - outerStart))
                total <- getNodeStackCount
                let go n
                     | n >= total = return []
                     | otherwise = do
                          node <- peekNode n
                          nBS <- readStr (name node)
                          (BS.unpack nBS :) <$> go (n+1)
                stackNames <- go 0
                error $ printf "Inconsistency detected in the node stack while parsing %s...\n Expected %s(%s), but obtained %s(%s) (and the stack contains %s)"
                                (BS.unpack xml) (BS.unpack nameBS_original) (show origStr) (BS.unpack seenBS) (show seenStr)(show stackNames)
  | otherwise =
      return ()

{-# INLINE checkConsistency #-}
{-# INLINE checkCursor #-}
