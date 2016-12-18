
{-# LANGUAGE OverloadedStrings#-}
import Data.ByteString.Xml
import Data.ByteString.Xml.Types
import Control.Monad
import Control.Lens
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
{-
childrenBy :: Node -> Str -> [Node]
childrenBy n s = n ^.. plate . filtered(\n' -> n' ^? details . _Element . name == Just s)

attributeBy :: Node -> Str -> Maybe Attribute
attributeBy n s = n ^? details._Element.attributes.each .filtered(\a -> nameA a == s)
-}
main = do
    forM_ examples $ \(parses,src) -> do
         case parse src of
            Left err ->
              when parses $ fail ("Unexpected failure on " ++ BS.unpack src ++ ": " ++ show err)
            Right doc -> do
              unless parses $ fail ( "Unexpected success on " ++ BS.unpack src)
              print src
              print doc
--     let Right doc = parse "<test id=\"1\" extra=\"2\" />\n<test id=\"2\" /><b><test id=\"3\" /></b><test id=\"4\" /><test />"
--     map name (children doc) === ["test","test","b","test","test"]
--     location (children doc !! 2) === (2,16)
--     length (childrenBy doc "test") === 4
--     length (childrenBy doc "b") === 1
--     length (childrenBy doc "extra") === 0
--     toList(attributes (head $ children doc)) ===
--       [Attribute "id" "1", Attribute "extra" "2"]
--     map (`attributeBy` "id") (childrenBy doc "test") === map (fmap (Attribute "id")) [Just "1", Just "2", Just "4", Nothing]

--     Right _ <- return $ parse $ "<test " <> BS.unwords [BS.pack $ "x" ++ show i ++ "='value'" | i <- [1..10000]] <> " />"
--     Right _ <- return $ parse $ BS.unlines $ replicate 10000 "<test x='value' />"

--     let attrs = ["usd:jpy","test","extra","more","stuff","jpy:usd","xxx","xxxx"]
--     Right doc <- return $ parse $ "<test " <> BS.unwords [x <> "='" <> x <> "'" | x <- attrs] <> ">middle</test>"
--     [c] <- return $ childrenBy doc "test"
--     forM_ attrs $ \a -> attributeBy c a === Just (Attribute a a)
--     forM_ ["missing","gone","nothing"] $ \a -> attributeBy c a === Nothing
--     putStrLn "\nSuccess"
    
-- a === b = if a == b then putChar '.' else fail $ "mismatch, " ++ show a ++ " /= " ++ show b
