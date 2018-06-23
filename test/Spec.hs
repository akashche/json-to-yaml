
{-# LANGUAGE OverloadedStrings #-}

import Prelude (IO, (==), head, map, mapM, return)
import Control.Monad (when)
import Data.Text.Lazy (Text, concat, pack)
import Data.Text.Lazy.IO (putStr, putStrLn)
import System.Directory (listDirectory)

import Files
import Parser
import Yaml

main :: IO ()
main = do
    let prefix = "test/JSONTestSuite/test_parsing/"
    list <- listDirectory prefix
    mapM (\en -> do
        let path = concat [(pack prefix), (pack en)]
        when
            ('y' == head en)
            (withFileText path (\text -> do
                putStr "\n\npath: "
                putStrLn path
                putStr "json: "
                putStrLn text
                putStr "yaml:"
                let val = parseJson text path
                putStrLn (toYaml 0 val))))
        list
    return ()
