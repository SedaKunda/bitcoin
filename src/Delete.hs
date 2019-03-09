module Delete 
    ( deleteData
    , deleteForDate
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

-- | deletes data from bitcoin database
deleteData :: Database 
           -> String 
           -> IO ()
deleteData db entity = do
   stmt <- prepare db (pack $ "DELETE FROM bitcoin WHERE currency=:entity")
   bindNamed stmt [ (pack ":entity", SQLText (pack entity)) ]
   step stmt
   column stmt 0
   return ()

-- | deletes data from bitcoin db - based on the date selected by user
deleteForDate :: Database
              -> String -- ^ date has type string
              -> IO()
deleteForDate db date = do
   stmt <- prepare db (pack $ "DELETE FROM bitcoin WHERE date=:date")
   bindNamed stmt [ (pack ":date", SQLText (pack date)) ]
   step stmt
   column stmt 0
   return () 
