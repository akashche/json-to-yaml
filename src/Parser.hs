
{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parseJson
    ) where

import Prelude
    ( Bool(True, False)
    , (>>)
    , const, either, id, read, return, show
    )

import Data.Text.Lazy (Text, pack, unpack)
import Text.Parsec ((<|>), char, many, many1, noneOf, oneOf, parse, sepBy, spaces)
import Text.Parsec.Char (digit, string)
import Text.Parsec.Text.Lazy (Parser)
import Data.Vector (fromList)

import Types

jsonNull = do
    spaces
    string "null"
    spaces
    return JNull

jsonFalse = do
    spaces
    string "false"
    spaces
    return (JBool False)

jsonTrue = do
    spaces
    string "true"
    spaces
    return (JBool True)

jsonNumber = do
    spaces
    num <- many1
        ( digit
        <|> oneOf [',', '.', '-']
        )
    spaces
    return (JNumber (read num))

jsonString = do
    spaces
    char '"'
    st <- many
        ( noneOf ['"', '\\']
        <|> (
            char '\\' >>
                ( char '"'
                <|> char '\\'
                <|> char '/'
                <|> char 'b'
                <|> char 'f'
                <|> char 'n'
                <|> char 'r'
                <|> char 't'
                -- <|> char 'u' TODO
                )
            )
        )
    char '"'
    spaces
    return (JString (pack st))

jsonArray = do
    char '['
    spaces
    li <- sepBy
        jsonValue
        (char ',')
    spaces
    char ']'
    return (JArray (fromList li))

jsonField :: Parser JField
jsonField = do
    spaces
    name <- many1 (noneOf [':'])
    spaces
    char ':'
    spaces
    val <- jsonValue
    spaces
    return (JField (pack name) val)

jsonObject = do
    spaces
    char '{'
    li <- sepBy
        jsonField
        (char ',')
    char '}'
    spaces
    return (JObject (fromList li))


jsonValue :: Parser JValue
jsonValue = do
    spaces
    val <-
        ( jsonNull
        <|> jsonTrue
        <|> jsonFalse
        <|> jsonNumber
        <|> jsonString
        <|> jsonArray
        <|> jsonObject
        )
    spaces
    return val

parseJson :: Text -> Text -> JValue
parseJson contents path =
    either
        (\e -> JString (pack (show e)))
        id
        (parse jsonValue (unpack path) contents)

