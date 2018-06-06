
{-# LANGUAGE OverloadedStrings #-}

module Files
    ( withFileText
    ) where

import Prelude (IO, return  )

import Data.ByteString.Lazy (hGetContents)
import Data.Text.Lazy (Text, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import System.IO (IOMode(ReadMode), withBinaryFile)

withFileText :: Text -> (Text -> IO a) -> IO a
withFileText path fun = withBinaryFile (unpack path) ReadMode (\ha -> do
    bs <- hGetContents ha
    let te = decodeUtf8 bs
    res <- fun te
    return res )
