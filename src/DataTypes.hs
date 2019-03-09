module DataTypes
    ( CryptoCompare (CryptoCompare)
    , Currencies (Currencies)
    ) where

import GHC.Int
-- | creates data type 'CryptoCompare' deriving Eq and Show
data CryptoCompare = CryptoCompare {
   currency :: String,
   date :: String,
   opening :: Double,
   high :: Double,
   low :: Double,
   closing :: Double,
   adj_close :: Double,
   volume :: Int64
} deriving (Eq, Show)
-- | creates data type 'Currencies' deriving Eq and Show
data Currencies = Currencies {
   iso_code :: String,
   currency_detail :: String,
   state :: String,
   fractional_unit :: String
} deriving (Eq, Show)
