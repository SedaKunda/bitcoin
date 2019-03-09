{-# LANGUAGE OverloadedStrings #-}
module AveragePrice
    ( averageClose
    , averageOpen
    ) where

-- http://hackage.haskell.org/package/direct-sqlite-2.3.23/docs/Database-SQLite3.html
import Database.SQLite3

import GHC.Int
import Data.Text (pack)
import Data.Conduit.Binary (sinkFile) -- Exported from the package conduit-extra
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString as B
import Conduit (runConduit, (.|))
import Control.Monad.Trans.Resource (runResourceT)
import qualified Control.Exception as E
import qualified Data.ByteString as D

-- | Calculates the average price for Bitcoin
averageClose :: Database 
             -> String -- ^ reads in cia which is type String 
             -> IO Double -- ^ outputs the average price which has type Double
averageClose db cia2 = do
   -- getting list of prices for bitcoin
   stmt <- prepare db (pack "SELECT (close) FROM bitcoin WHERE currency=:cia2")
   bindNamed stmt [ (pack ":cia2", SQLText (pack cia2)) ]
   let
       isFloat (SQLFloat _) = True
       isFloat _ = False
   let getFloat (SQLFloat f) = f
   let readPrice ps = do
         result <- step stmt       -- one statement step
         p <- column stmt 0        -- read price
         if isFloat p then
            readPrice (p:ps)
         else
            return ps
   ps <- readPrice []
   let fs = map getFloat ps
   return $ (sum fs) / (read.show.length $ fs)


averageOpen :: Database 
             -> String -- ^ reads in cia which is type String 
             -> IO Double -- ^ outputs the average price which has type Double
averageOpen db cia2 = do
   -- getting list of prices for bitcoin
   stmt <- prepare db (pack "SELECT (open) FROM bitcoin WHERE currency=:cia2")
   bindNamed stmt [ (pack ":cia2", SQLText (pack cia2)) ]
   let
       isFloat (SQLFloat _) = True
       isFloat _ = False
   let getFloat (SQLFloat f) = f
   let readPrice ps = do
         result <- step stmt       -- one statement step
         p <- column stmt 0        -- read price
         if isFloat p then
            readPrice (p:ps)
         else
            return ps
   ps <- readPrice []
   let fs = map getFloat ps
   return $ (sum fs) / (read.show.length $ fs)
