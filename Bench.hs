{-# LANGUAGE OverloadedStrings #-}
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

import System.IO
import System.Environment

import Text.Printf
import qualified Text.XML as XML
import Text.XML.Light

import Criterion
import Criterion.Main

import GHC.Generics (Generic)

process :: Show a => Either a b -> String
process = either (error.show) (const "Success")

main = do
  paths <- getArgs

  let suite =
        [ env ((,) <$> BS.readFile p <*> readFile p) $ \ ~(bs,s) -> bgroup p
            [ bench "hexml" $ whnf (process . Hexml.parse) bs
            , bench "bytestring-xml" $ whnf (process . parse) bs
            , bench "xml" $ nf parseXML s
            , bench "xml-conduit" $ whnfIO $ XML.readFile def p
            ]
        | p <- paths ]

  defaultMain suite 

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
