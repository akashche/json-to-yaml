
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types
    ( JValue(..)
    , JField(..)
    ) where

import Prelude (Bool, Double, Show)
import Data.Text.Lazy (Text)
import Data.Vector (Vector)


data JValue = JNull
    | JBool Bool
    | JNumber Double
    | JString Text
    | JArray (Vector JValue)
    | JObject (Vector JField)
    deriving Show

data JField = JField
    { name :: Text
    , value :: JValue
    } deriving Show

