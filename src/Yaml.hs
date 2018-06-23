
{-# LANGUAGE OverloadedStrings #-}

module Yaml
    ( toYaml
    ) where

import Prelude (Int, (+), otherwise, replicate, show)

import Data.Monoid ((<>))
import Data.Text.Lazy (Text, pack)
import Data.Text.Lazy.Builder (Builder, fromLazyText, fromString, toLazyText)
import Data.Vector (ifoldl')

import Types

toYaml :: Int -> JValue -> Text

toYaml _ (JNull) = ""

toYaml _ (JBool val)
    | val = "true"
    | otherwise = "false"

toYaml _ (JNumber val) = pack (show val)

toYaml _ (JString val) = val

toYaml indent (JArray val) =
    toLazyText
        (ifoldl'
            (\ac i el ->
                ac
                <> (fromLazyText "\n")
                <> (fromString (replicate indent ' '))
                <> (fromLazyText "- ")
                <> (fromLazyText (toYaml (indent + 2) el))
                )
            (fromLazyText "")
            val )

toYaml indent (JObject val) =
    toLazyText
        (ifoldl'
            (\ac i el ->
                ac
                <> (fromLazyText "\n")
                <> (fromString (replicate indent ' '))
                <> (fieldToYaml (indent + 2) el)
                )
            (fromLazyText "")
            val)

fieldToYaml :: Int -> JField -> Builder
fieldToYaml indent (JField name val) =
       (fromLazyText name)
    <> (fromLazyText ": ")
    <> (fromLazyText (toYaml indent val))

