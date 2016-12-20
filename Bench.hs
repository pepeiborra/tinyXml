{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving, DeriveGeneric #-}
import Data.ByteString.Xml
import Control.DeepSeq
import Control.Monad
import Data.Default
import Data.Foldable
import Data.Maybe
import Data.Monoid
import qualified Data.ByteString.Char8 as BS
import qualified Text.XML.Hexml as Hexml

import System.Process (callCommand)
import System.IO
import System.Environment

import Text.Printf
import qualified Text.XML as XML
import Text.XML.Light

import Criterion
import Criterion.Main

import GHC.Generics (Generic)


main = do
  paths0 <- getArgs
  paths <- case paths0 of
    _ : _ -> return paths0
    [] -> do
      callCommand "tar xf benchmark.tar.bz"
      return ["benchmark.xml"]
  let makeBenchmark :: String -> (forall a. NFData a => (a->c) -> a -> Benchmarkable) -> (forall a b .(Show a, Show b) => Either a b -> c) -> Benchmark
      makeBenchmark name force process =
       bgroup name $
        [ env ((,) <$> BS.readFile p <*> readFile p) $ \ ~(bs,s) -> bgroup p
            [
              bench "bytestring-xml" $ force (process . parse) bs
            , bench "hexml" $ force (process . Hexml.parse) bs
--            , bench "xml" $ whnf (length . parseXML) s
         --   , bench "xml-conduit" $ whnfIO $ XML.readFile def p
            ]
        | p <- paths]

  let validate   = makeBenchmark "validate"    whnf $ either (error.show) (const "Success")
  let fullyForce = makeBenchmark "fully force" nf   $ either (error.show) show

  defaultMain [validate, fullyForce]

deriving instance Generic Content
deriving instance Generic Element
deriving instance Generic CData
deriving instance Generic CDataKind
deriving instance Generic QName
deriving instance Generic Attr
instance NFData Content
instance NFData Element
instance NFData CData
instance NFData CDataKind
instance NFData QName
instance NFData Attr
