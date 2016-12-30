
{-# LANGUAGE OverloadedStrings, DisambiguateRecordFields #-}

import Data.ByteString.Xml hiding (length)
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Monoid
import qualified Data.ByteString.Char8 as BS


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
    putStrLn "\nSuccess"

checkFind :: Node -> IO ()
checkFind n = do
    forM_ (attributes n) $ \a -> attributeBy n (attributeName a) === Just a
    attributeBy n "xxx" === Nothing
    let cs = children n
    forM_ ("xxx":map name cs) $ \c ->
        map outer (filter ((==) c . name) cs) === map outer (childrenBy n c)
    mapM_ checkFind $ children n


a === b = if a == b then putChar '.' else fail $ "mismatch, " ++ show a ++ " /= " ++ show b

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
