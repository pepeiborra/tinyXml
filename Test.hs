
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings, DisambiguateRecordFields #-}

import qualified Text.XML.Hexml as Hexml
import Control.Monad
import Data.Char
import Data.Foldable
import Data.List (sort)
import Data.Monoid
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector.Storable as V
import System.Process (callCommand)
import System.FilePath
import Text.Printf

import Config
import Text.Xml.Tiny
import Text.Xml.Tiny.Internal(Node(..), Attribute(..), ParseDetails(ParseDetails), AttributeParseDetails(..), Slice)
import qualified Text.Xml.Tiny.Internal as Slice

examples :: [(Bool, BS.ByteString)]
examples =
    [(True,"<hello>world</hello>")
    ,(True,"<hello/>")
    ,(True, "<test id='bob'>here<extra/>there</test>")
    ,(True, "<test /><close />")
    ,(True, "<test /><!-- comment > --><close />")
    ,(True, "<test id=\"bob value\" another-attr=\"test with <\">here </test> more text at the end<close />")
    ,(False, "<test></more>")
    ,(False, "<test")
    ,(True, "<?xml version=\"1.1\"?>\n<greeting>Hello, world!</greeting>")
    ]

xmlFiles = [ "mail", "benchmark" ]

main = do
    forM_ examples $ \(parses,src) -> do
         case parse src of
            Left err ->
              when parses $ fail ("Unexpected failure on " ++ BS.unpack src ++ ": " ++ show err)
            Right doc -> do
              unless parses $ fail ( "Unexpected success on " ++ BS.unpack src)
              print src
              print doc

    let Right doc = parse "<test id=\"1\" extra=\"2\" />\n<test id=\"2\" /><b><test id=\"3\" /></b><test id=\"4\" /><test />"
    map name (children doc) === ["test","test","b","test","test"]
    location (children doc !! 2) === (2,16)
    length (childrenBy doc "test") === 4
    length (childrenBy doc "b") === 1
    length (childrenBy doc "extra") === 0
    attributes (head $ children doc) === [Attribute "id" "1", Attribute "extra" "2"]
    map (`attributeBy` "id") (childrenBy doc "test") === map (fmap (Attribute "id")) [Just "1", Just "2", Just "4", Nothing]

    Right _ <- return $ parse $ "<test " <> BS.unwords [BS.pack $ "x" ++ show i ++ "='value'" | i <- [1..10000]] <> " />"
    Right _ <- return $ parse $ BS.unlines $ replicate 10000 "<test x='value' />"

    let attrs = ["usd:jpy","test","extra","more","stuff","jpy:usd","xxx","xxxx"]
    Right doc <- return $ parse $ "<test " <> BS.unwords [x <> "='" <> x <> "'" | x <- attrs] <> ">middle</test>"
    [c] <- return $ childrenBy doc "test"

    forM_ attrs $ \a -> attributeBy c a === Just (Attribute a a)
    forM_ ["missing","gone","nothing"] $ \a -> attributeBy c a === Nothing

    forM_ xmlFiles $ \name -> do
      putStrLn ""

      let path   = "xml" </> name <.> "xml"
      let pathGz = path <.> ".bz2"
      callCommand $ "bunzip2 -f -k " ++ pathGz
      xml <- BS.readFile path
      let us = either (error $ "failed to parse: " ++ path) id $ parse xml

      checkStructure us

      let hexml = either (error $ "Hexml failed to parse: " ++ path ) id $ Hexml.parse xml
      testEq us hexml

    putStrLn "\nSuccess"

checkFind :: Node -> IO ()
checkFind n = do
    forM_ (attributes n) $ \a -> attributeBy n (attributeName a) === Just a
    attributeBy n "xxx" === (Nothing :: Maybe Attribute)
    let cs = children n
    forM_ ("xxx":map name cs) $ \c ->
        map outer (filter ((==) c . name) cs) === map outer (childrenBy n c)
    mapM_ checkFind $ children n

pairs f (a:b:rest) = f a b && pairs f (b:rest)
pairs f _ = True

checkStructure :: Config => Node -> IO ()
checkStructure n = checkNode [] n where
  checkNode path n@Node{attributesV, slices=ParseDetails{attributes}} = do
    let nn = children n
    unless (sorted nn) $ fail "not sorted"
    unless (pairs (nonOverlapping path) nn) $ fail "overlapping children nodes"
    unless (pairs nonOverlappingA (Slice.vector attributes attributesV)) $ fail "overlapping attributes"
    putChar '.'
    forM_ nn $ \n' -> checkNode (name n : path) n'

  nonOverlapping :: Config => [BS.ByteString] -> Node -> Node -> Bool
  nonOverlapping path n1@Node{slices=ParseDetails{outer=o1}} n2@Node{slices=ParseDetails{outer=o2}} =
    nonOverlappingS o1 o2
    || error (printf "%s Overlapping nodes: %s(%s) %s(%s)" (show path) (show$ outer n1) (show $ location n1) (show$ outer n2) (show$ location n2))

  nonOverlappingA :: Config => AttributeParseDetails -> AttributeParseDetails -> Bool
  nonOverlappingA a1@(AttributeParseDetails n v) a2@(AttributeParseDetails n' v') =
    let slices = [n,v,n',v']
    in  and [ s >= s' || nonOverlappingS s s'
              | s <- slices, s' <- slices]
        || error (printf "overlapping attributes" (show a1) (show a2))

  nonOverlappingS :: Config => Slice -> Slice -> Bool
  nonOverlappingS s1 s2 =    Slice.end s1 <= Slice.start s2
                          || Slice.end s2 <= Slice.start s1
                          -- || error (printf "Overlapping slices: %s, %s" (show s1) (show s2))

  sorted nn =
    let outers = map (Slice.start.Slice.outer.slices) nn
    in sort outers == outers
    || error ("Internal error - nodes not sorted: " ++
              show [ (name n, Slice.start(Slice.outer(slices n))) | n <- nn])

class (Show a, Show b) => TestEq a b where testEq :: a -> b -> IO ()

(===) :: Config => TestEq a a => a -> a -> IO ()
(===) = testEq

instance (Show a, Eq a) => TestEq a a where
  a `testEq` b = if a == b then putChar '.' else error $ "mismatch, " ++ show a ++ " /= " ++ show b

instance TestEq Node Hexml.Node where
  testEq n n' = do
    name n `testEq` Hexml.name n'
    test "attributes" (attributes n) (Hexml.attributes n')
    test "contents"   (contents n)   (Hexml.contents n')

   where
     test (msg :: String) aa bb
       | length aa == length bb = zipWithM_ testEq aa bb
       | otherwise = error$ printf "Length of %s does not match (%d /= %d):\n%s\n---------------\n%s" msg (length aa) (length bb) (show aa) (show bb)

instance TestEq Attribute Hexml.Attribute where
    Attribute n v `testEq` Hexml.Attribute n' v' = do
      n `testEq` n'
      v `testEq` v'

instance (Show a, Show b, TestEq a a', TestEq b b') => TestEq (Either a b) (Either a' b') where
  Left  e `testEq` Left e'  = e `testEq` e'
  Right x `testEq` Right x' = x `testEq` x'
  testEq a b = error $ printf "mismatch in children: %s /= %s" (show a) (show b)

debugShow :: Node -> String
debugShow n =
    unlines $
         "Nodes buffer: "
       : [ "  " ++ show n | n <- V.toList $ nodesV n]
       ++ showNodeContents (Right n)
     where
       showNodeContents :: Either BS.ByteString Node -> [String]
       showNodeContents (Right n) =
          [ "Node contents:"
          , "  name: " ++ show (name n)
          , "  slices: " ++ show (slices n)
          , "  attributes: " ++ (show $ attributes n)
          , "  contents: "
          ] ++
          [ "    " ++ l | n' <- contents n, l <- showNodeContents n']
       showNodeContents (Left txt) =
          [ "Text content: " ++ BS.unpack txt ]
