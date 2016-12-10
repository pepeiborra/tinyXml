{-# LANGUAGE OverloadedStrings#-}
import Data.ByteString.Xml
import Control.Monad
import Data.Foldable
import Data.Monoid
import qualified Data.ByteString.Char8 as BS
import qualified Text.XML.Hexml as Hexml

import System.IO
import System.Environment

import Text.Printf

import Criterion
import Criterion.Main

process :: Show a => Either a b -> String
process = either show (const "Success")

main = do
  paths <- getArgs

  contents <- mapM BS.readFile paths
  
  let suite =
        [ bgroup p
            [ bench "hexml" $ whnf (process . Hexml.parse) contents
            , bench "us" $ whnf (process . parse) contents
            ]
        | (p,contents) <- zip paths contents ]

  defaultMain suite 
