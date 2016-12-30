{-# LANGUAGE OverloadedStrings#-}
import Data.ByteString.Xml
import Control.Monad
import qualified Data.ByteString.Char8 as BS

import System.IO
import System.Environment

import Text.Printf

process :: Show a => Either a b -> String
process = either show (const "Success")

main = do
  paths <- getArgs
  hSetBuffering stdout LineBuffering
  forM_ paths $ \p -> do
    contents <- BS.readFile p
    printf "Us: %s\n" $ process $ parse contents
