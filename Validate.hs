{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import Text.Xml.Tiny
import Control.Monad
import qualified Data.ByteString.Char8 as BS

import Options.Generic
import System.Directory
import System.FilePath
import System.IO
import System.Environment

import Text.Printf

data Options =
  Options { reprint :: Bool <?> "Reprint the XML document" }
  deriving (Generic, Show, ParseRecord)

data Args = Args Options [String] deriving (Generic, Show)

instance ParseRecord Args where parseRecord = Args <$> parseRecord <*> parseRecord

main = do
  Args args paths <- getRecord "Validate XML"
  hSetBuffering stdout LineBuffering
  forM_ paths $ \p -> do
    contents <- BS.readFile p
    case parse contents of
      Left e -> print e
      Right node -> do
        putStrLn "OK"
        when (unHelpful $ reprint args) $ do
          let destPath = p <.> "reprint"
          existsAlready <- doesFileExist destPath
          if existsAlready
            then putStrLn $ "Overwriting " ++ destPath
            else putStrLn $ "Reprinting to " ++ destPath
          BS.writeFile destPath (rerender node)
