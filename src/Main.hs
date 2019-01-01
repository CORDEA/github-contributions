{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Network.HTTP.Base ( defaultUserAgent )
import Network.URI ( parseURI )
import Data.ByteString.Lazy  ( ByteString )
import Data.Text.Lazy.Encoding
import Text.HTML.TagSoup
import qualified Data.Text.Lazy as T
import qualified Parser

url :: String
url = "https://github.com/users/CORDEA/contributions"

sendRequest :: IO ( Response ByteString )
sendRequest =
    newManager tlsManagerSettings >>= \m ->
        httpLbs req { method = "GET" } m
    where
        Just req = parseUrlThrow url

fetched :: Response ByteString -> IO ()
fetched response =
    putStrLn $ unwords parsed
    where
        resp = responseBody response
        tags = parseTags $ T.unpack $ decodeUtf8 resp
        parsed = Parser.parse tags

main :: IO ()
main = fetched =<< sendRequest
