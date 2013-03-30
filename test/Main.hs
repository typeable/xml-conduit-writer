{-# LANGUAGE OverloadedStrings #-}
import Text.XML.Writer

import qualified Data.Text as T

-- Serializing is easy!
data ExampleADT = NullaryExample
                | UnaryExample String
                | RecordExample { earFoo :: Int
                                , earBar :: Maybe Bool
                                , earBaz :: [Float]
                                }

instance ToXML ExampleADT where
    toXML NullaryExample = empty
    toXML (UnaryExample s) = element "unary" $ content (T.pack s)
    toXML (RecordExample {earFoo = foo, earBar = bar, earBaz = baz}) =
        element "record" $ do
            element "foo" $ toXML foo
            element "bar" $ toXML bar
            element "baz" $ many "fnord" baz

main :: IO ()
main = do
    pprint $ document "root" $ do
        element "{ns:uri}pseudo:prefix" $ do
            element "unprefixed" "empty NS"
            element "pseudo:prefixed" $ comment "wrong!"

        element ("sns" !: "{silly:ns:uri}spam") $ do
            comment "looks good?"
            elementA "unprefixed" [("with", "attrs"), ("empty", "body")] empty

        element "salad" $ do
            content "eggs"
            content "bacon"
            comment "Like a county in England"

        instruction "php" "echo('goodbye, world!')"

    pprint $ soap () $ do
        element ("v" !: "{vendor:uri}request") $ do
            element "complex" $ do
                element "key" "value"
                elementA "tag" [("key", "value")] empty
            element "text" $ content "some text"
            element "bool" $ toXML True
            element "float" $ toXML (42 :: Float)
            element "int" $ toXML (42 :: Int)
            element "char" $ toXML 'Ч'

    pprint $ document ("adt" !: "{org.haskell.text.xml.monad.ExampleADT}example") $ do
        element "void" $ toXML NullaryExample
        toXML $ UnaryExample "hi!"
        toXML $ RecordExample { earFoo = 9000 + 1
                              , earBar = Nothing
                              , earBaz = [1, 2, 3]
                              }
