module Retrieve 
    ( openForDate
    , closeForDate
    , currencyInfo
    ) where

import Database.SQLite3
import GHC.Int
import DataTypes
import Data.Text (pack, unpack)
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

-- | a query to retrieve open value for an inputted date

openForDate :: Database 
            -> String -- ^ reads in the date(YYYY-MM-DD) which has type String 
            -> IO Double -- ^ outputs open value which has type IO Double
openForDate db date = do
   stmt <- prepare db (pack $ "SELECT (open) FROM bitcoin WHERE date=:date")
   bindNamed stmt [ (pack ":date", SQLText (pack date)) ]
   result <- step stmt       -- one statement step
   openV <- column stmt 0   -- read how returned
   let readOpen (SQLFloat n) = n
       readOpen _ = 0
   let openVal = readOpen openV
   return openVal

-- | a query to retrieve close value for an inputted date
closeForDate :: Database 
             -> String -- ^ reads in the date(YYYY-MM-DD) which has type String
             -> IO Double -- ^ outputs open value which has type IO Double
closeForDate db date = do
   stmt <- prepare db (pack $ "SELECT (close) FROM bitcoin WHERE date=:date")
   bindNamed stmt [ (pack ":date", SQLText (pack date)) ]
   result <- step stmt       -- one statement step
   closeV <- column stmt 0   -- read how returned
   let readClose (SQLFloat n) = n
       readClose _ = 0
   let closeVal = readClose closeV
   return closeVal
   
-- | a query to retrieve currency details
currencyInfo :: Database 
             -> String
             -> IO [String] -- ^ outputs open value which has type IO String
currencyInfo db date = do
   stmt <- prepare db (pack $ "SELECT state, fractional_unit, c.currency FROM bitcoin b JOIN currencies c ON b.currency=c.iso_code WHERE date=:date")
   bindNamed stmt [ (pack ":date", SQLText (pack date)) ]
   result <- step stmt       -- one statement step
   st <- column stmt 0   -- read how returned
   fr <- column stmt 1   -- read how returned
   cu <- column stmt 2   -- read how returned
   let readS (SQLText n) = unpack n
       readS _ = "state"
   let readF (SQLText n) = unpack n
       readF _ = "fractional_unit"
   let readC (SQLText n) = unpack n
       readC _ = "c.currency"
   let cVal = readC cu
   let fVal = readF fr
   let sVal = readS st
   let summary = fVal ++ cVal ++ sVal
   return ["Currency: " ++ cVal, "Fractional unit: " ++ fVal, "State: " ++ sVal]

