module Main where

import HsonSchema

import qualified Data.Aeson as Json
import Data.Aeson.Encode.Pretty(encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Foldable(sequence_)
import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    if length args == 0 then
        putStrLn "Usage: <json-file-path>"
    else do
        handle <- openBinaryFile (head args) ReadMode
        byteString <- BS.hGetContents handle
        sequence_ $ fmap (BS.putStrLn . encodePretty . makeSchema) $ (Json.decode byteString :: Maybe Json.Value)
        hClose handle
        return ()
