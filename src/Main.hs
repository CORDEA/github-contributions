{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Network.HTTP.Base ( defaultUserAgent )
import Network.URI ( parseURI )
import Data.ByteString.Lazy  ( ByteString )
import Data.Text.Lazy.Encoding
import Text.HTML.TagSoup
import Options.Applicative
import Data.Semigroup ((<>))
import Contribution
import qualified Data.Text.Lazy as T
import qualified HTMLParser

url :: String
url = "https://github.com/users/"

contributionsPath :: String
contributionsPath = "/contributions"

data Args = Args {
    user :: String,
    date :: String
    }

commandParser :: Parser Args
commandParser = Args
    <$> strOption
        ( long "user"
        <> short 'u'
        <> help "GitHub User" )
    <*> strOption
        ( long "date"
        <> short 'd'
        <> help "Date"
        <> value "" )

parser :: ParserInfo Args
parser = info commandParser
    ( progDesc "Contribution Command" )

toQuery :: String -> String
toQuery [] = ""
toQuery query = "?to=" ++ query

buildUrl :: Args -> String
buildUrl ( Args user date ) =
    url ++ user ++ contributionsPath ++ toQuery date

sendRequest :: Args -> IO ( Response ByteString )
sendRequest args =
    newManager tlsManagerSettings >>= \m ->
        httpLbs req { method = "GET" } m
    where
        Just req = parseUrlThrow $ buildUrl args

fetched :: Response ByteString -> IO ()
fetched response =
    putStrLn $ unlines $ map ( forOutput ) parsed
    where
        resp = responseBody response
        tags = parseTags $ T.unpack $ decodeUtf8 resp
        parsed = HTMLParser.parse tags

parsed :: Args -> IO ()
parsed args = fetched =<< sendRequest args

main :: IO ()
main = parsed =<< execParser parser
