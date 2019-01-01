{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Network.HTTP.Base ( defaultUserAgent )
import Network.URI ( parseURI )
import Data.ByteString.Lazy  ( ByteString )
import Data.Text.Lazy
import Data.Text.Lazy.Encoding

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
    putStrLn $ unpack $ decodeUtf8 resp
    where
        resp = responseBody response

main :: IO ()
main = fetched =<< sendRequest
