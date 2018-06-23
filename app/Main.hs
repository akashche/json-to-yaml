
{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import Prelude (IO, head)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.IO (putStrLn)
import System.Environment (getArgs)

import Files
import Parser
import Yaml

main :: IO ()
main = do
    args <- getArgs
    let path = pack (head args)
    withFileText path (\text -> do
        let val = parseJson text path
        putStrLn (toYaml 0 val))

