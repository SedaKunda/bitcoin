{-# LANGUAGE OverloadedStrings #-}
module Crypto
    ( getCrypto
    ) where

import Insert
import Network.HTTP.Conduit
import Data.Time.Clock
import Data.Time.Calendar
import qualified Control.Exception as E
import qualified Data.ByteString.Lazy.Internal as L
import Network.HTTP.Types.Status (statusCode)

type URL = String

-- | UTCTime
past :: UTCTime
past = UTCTime (ModifiedJulianDay 56200) (secondsToDiffTime 0)

future :: UTCTime
future = UTCTime (ModifiedJulianDay 562000) (secondsToDiffTime 0)

-- | Cookie for the web link the dataset will be downloaded from
cookie :: Cookie
cookie = Cookie { cookie_name = "B"
                , cookie_value = "8d5oj31dn8v1s&b=3&s=5d"
                , cookie_expiry_time = future
                , cookie_domain = "query1.finance.yahoo.com"
                , cookie_path = "/"
                , cookie_creation_time = past
                , cookie_last_access_time = past
                , cookie_persistent = False
                , cookie_host_only = False
                , cookie_secure_only = False
                , cookie_http_only = False
                }

-- | HTTP request to download dataset
getCrypto :: URL -> IO L.ByteString
getCrypto cia = do
    request' <- parseRequest $ "https://query1.finance.yahoo.com/v7/finance/download/" ++ cia ++ "?period1=1510272000&period2=1512864000&interval=1d&events=history&crumb=UD.SDzP6dyv"
    manager <- newManager tlsManagerSettings
    let request = request' { cookieJar = Just $ createCookieJar [cookie] }
    response <- httpLbs request manager
    return $ responseBody response
