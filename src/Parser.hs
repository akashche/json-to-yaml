
{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parseJson
    ) where

import Prelude
    ( Bool(True, False)
    , Double
    , Int
    , Integer
    , (+)
    , (*)
    , (/)
    , (^)
    , (>>)
    , const
    , either
    , flip
    , fromIntegral
    , fst
    , id
    , length
    , not
    , read
    , return
    , show
    , snd
    )

import Control.Monad (when)
import Data.Char (chr)
import Data.Text.Lazy (Text, pack, unpack)
import Data.Vector (fromList)
import Text.Parsec
    ( (<|>)
    , (<?>)
    , char
    , count
    , hexDigit
    , many
    , many1
    , noneOf
    , oneOf
    , option
    , optional
    , parse
    , sepBy
    , skipMany
    , spaces
    )
import Text.Parsec.Char (digit, string)
import Text.Parsec.Text.Lazy (Parser)

import Types

-- helpers

-- lexeme may be used instead
whitespace :: Parser ()
whitespace = skipMany (oneOf [' ', '\t', '\n', '\r'])

-- parsers

jsonNull = do
    string "null"
    whitespace
    return JNull

jsonFalse = do
    string "false"
    whitespace
    return (JBool False)

jsonTrue = do
    string "true"
    whitespace
    return (JBool True)

-- use "read" only for integers for better control
jsonNumber = do
    -- sign
    negative <- option
        False
        (char '-' >> return True)
    -- before dot
    mantissaHead <- many1 digit
    -- after dot
    mantissaTail <- option
        "0"
        (char '.' >> many1 digit)
    -- exp part
    exptup <- option
        (False, 0)
        (do
            oneOf ['e', 'E']
            expneg <- option
                False
                (char '-' >> return True)
            when (not expneg)
                (optional (char '+'))
            expstr <- many1 digit
            let expnum = read expstr :: Integer
            return (expneg, expnum))
    whitespace
    -- compute
    let head = (read mantissaHead) :: Double
    let tail = (read mantissaTail ) :: Double
    let divider = (fromIntegral (10 ^ (length mantissaTail))) :: Double
    let real = head + (tail / divider)
    let expOp = if (fst exptup)
        then (/)
        else (*)
    let expval = (fromIntegral (10 ^ (snd exptup))) :: Double
    let absval = expOp real expval
    let numval = if negative
        then (- absval)
        else absval
    return (JNumber numval)

jsonString = do
    char '"'
    st <- many
        ( noneOf ['"', '\\']
        <|> (
            char '\\' >>
                (   char '"'
                <|> char '\\'
                <|> char '/'
                <|> (char 'b' >> return '\b')
                <|> (char 'f' >> return '\f')
                <|> (char 'n' >> return '\n')
                <|> (char 'r' >> return '\r')
                <|> (char 't' >> return '\t')
                <|> (char 'u' >> (do
                    hexstr  <- count 4 hexDigit
                    let num = read ('0':('x':hexstr)) :: Int
                    return (chr num)))
                )
            )
        )
    char '"'
    whitespace
    return (JString (pack st))

jsonArray = do
    char '['
    whitespace
    li <- sepBy
        jsonValue
        ((char ',') >> whitespace)
    whitespace
    char ']'
    whitespace
    return (JArray (fromList li))

jsonField :: Parser JField
jsonField = do
    name <- jsonString
    whitespace
    char ':'
    whitespace
    val <- jsonValue
    whitespace
    return (JField ((\(JString x) -> x) name) val)

jsonObject = do
    char '{'
    whitespace
    li <- sepBy
        jsonField
        ((char ',') >> whitespace)
    whitespace
    char '}'
    whitespace
    return (JObject (fromList li))


jsonValue :: Parser JValue
jsonValue = do
    whitespace
    val <-
        (   jsonNull
        <|> jsonTrue
        <|> jsonFalse
        <|> jsonNumber
        <|> jsonString
        <|> jsonArray
        <|> jsonObject
        <?> "JSON_VALUE"
        )
    return val

parseJson :: Text -> Text -> JValue
parseJson contents path =
    either
        (\e -> JString (pack (show e)))
        id
        (parse jsonValue (unpack path) contents)

