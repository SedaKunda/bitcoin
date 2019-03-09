{-# LANGUAGE OverloadedStrings #-}
module Insert
    ( database
    , initialiseDB
    , batchInsert
    ) where

import Database.SQLite3
import GHC.Int
import DataTypes
import Data.Text (pack)
import Data.Conduit.Binary (sinkFile) -- Exported from the package conduit-extra
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString as B
import Network.HTTP.Conduit
import Network.HTTP.Types.Status (statusCode)
import Conduit (runConduit, (.|))
import Control.Monad.Trans.Resource (runResourceT)
import qualified Control.Exception as E
import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.ByteString as D

-- | connect to db
database :: IO Database
database = open $ pack "bitcoin.db"

-- | creates 2 tables for currencies and bitcoin
initialiseDB :: Database -> IO ()
initialiseDB db = exec db $ pack "CREATE TABLE IF NOT EXISTS currencies (\
            \iso_code VARCHAR(10) NOT NULL, \
            \currency VARCHAR(40) NOT NULL, \
            \state VARCHAR(50) NOT NULL, \
            \fractional_unit VARCHAR(40) NOT NULL, UNIQUE (iso_code));\
            \CREATE TABLE IF NOT EXISTS bitcoin (\
            \currency VARCHAR(40) NOT NULL, \
            \date VARCHAR(40) NOT NULL, \
            \open FLOAT DEFAULT NULL, \
            \high FLOAT DEFAULT NULL, \
            \low FLOAT DEFAULT NULL, \
            \close FLOAT DEFAULT NULL, \
			\adj_close VARCHAR(40) DEFAULT NULL,\
            \volume BIGINT(11) DEFAULT NULL)"

-- | insert statement for bitcoin table
insertBitcoin :: Database 
              -> String -- ^ reads in s which has type String
              -> CryptoCompare -- ^ currency has data defined type CyptoCompare
              -> IO ()
insertBitcoin db s currency = do
   let f (CryptoCompare s d o h l c a v) = [ (pack ":s", SQLText (pack s))
                                   , (pack ":d", SQLText (pack d))
                                   , (pack ":o", SQLFloat o)
                                   , (pack ":h", SQLFloat h)
                                   , (pack ":l", SQLFloat l)
                                   , (pack ":c", SQLFloat c)
                                   , (pack ":a", SQLFloat a)
                                   , (pack ":v", SQLInteger v)
                                   ]
   let args = f currency
   stmt <- prepare db (pack "INSERT INTO bitcoin VALUES (:s,:d,:o,:h,:l,:c,:a,:v)")
   bindNamed stmt (f currency)
   result <- step stmt
   return ()

-- | insert statement for currency table
insertCurrencies :: Database -> IO ()
insertCurrencies db = do
   stmt2 <- prepare db (pack "INSERT OR IGNORE INTO currencies VALUES ('GBP', 'Great British Pound', 'United Kingdom', 'Penny'), (\
           \'USD', 'United States Dollar', 'United States of America', 'Cents'), (\
           \'EUR', 'Euro', 'Europe', 'Cents'), (\
           \'JPY', 'Japanese Yen', 'Japan', 'Sen'), (\
           \'CHF', 'Swiss Franc', 'Switzerland', 'Rappen'), (\
           \'CAD', 'Canadian Dollar', 'Canada', 'Cents'), (\
           \'AUD', 'Australian Dollar', 'Australia', 'Cents')")
   result <- step stmt2
   return ()

-- | batch insert to db
batchInsert :: String 
            -> [CryptoCompare] -- ^ batchInsert inserts list of currency with data defined type CryptoCompare 
            -> IO ()
batchInsert currency prices = do
   db <- database
   initialiseDB db
   insertCurrencies db
   mapM_ (insertBitcoin db currency) prices

