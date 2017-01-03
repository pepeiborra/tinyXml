{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
import Text.XML.Hexml
import Control.Monad
import qualified Data.ByteString.Char8 as BS

import Data.Char
import Data.Monoid

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

rerender :: Node -> BS.ByteString
rerender = inside
    where
        inside x = BS.concat $ map (either validStr node) $ contents x
        node x = "<" <> BS.unwords (validName (name x) : map attr (attributes x)) <> ">" <>
                 inside x <>
                 "</" <> name x <> ">"
        attr (Attribute a b) = validName a <> "=\"" <> validAttr b <> "\""

        validName x | BS.all (\x -> isAlphaNum x || x `elem` ("-:_" :: String)) x = x
                    | otherwise = error "Invalid name"
        validAttr x | BS.notElem '\"' x = x
                    | otherwise = error "Invalid attribute"
        validStr x | BS.notElem '<' x || BS.isInfixOf "<!--" x = x
                   | otherwise = error $ show ("Invalid string", x)
