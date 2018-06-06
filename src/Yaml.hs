
{-# LANGUAGE OverloadedStrings #-}

module Yaml
    ( toYaml
    ) where

import Prelude (otherwise, show)

import Data.Monoid ((<>))
import Data.Text.Lazy (Text, pack)
import Data.Text.Lazy.Builder (Builder, fromLazyText, toLazyText)
import Data.Vector (ifoldl')

import Types

toYaml :: JValue -> Text

toYaml (JNull) = ""

toYaml (JBool val)
    | val = "true"
    | otherwise = "false"

toYaml (JNumber val) = pack (show val)

toYaml (JString val) = val

toYaml (JArray val) =
    toLazyText
        (ifoldl'
            (\ac i el ->
                ac
                <> (fromLazyText " - ")
                <> (fromLazyText (toYaml el))
                <> (fromLazyText "\n")
                )
            (fromLazyText "")
            val )

toYaml (JObject val) =
    toLazyText
        (ifoldl'
            (\ac i el ->
                ac
                <> (fieldToYaml el)
                <> (fromLazyText "\n")
                )
            (fromLazyText "")
            val)

fieldToYaml :: JField -> Builder
fieldToYaml (JField name val) =
    (fromLazyText " - ")
    <> (fromLazyText name)
    <> (fromLazyText " : ")
    <> (fromLazyText (toYaml val))

