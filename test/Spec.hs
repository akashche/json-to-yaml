
{-# LANGUAGE OverloadedStrings #-}

import Prelude (IO)
import Data.Text.Lazy.IO (putStr)

import Files
import Parser
import Yaml

main :: IO ()
main = do
    let path = "test/test.json"
    withFileText path (\text -> do
        let val = parseJson text path
        putStr (toYaml val))
