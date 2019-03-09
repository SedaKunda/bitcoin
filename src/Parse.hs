{-# LANGUAGE OverloadedStrings #-}
module Parse
    ( parse
    , parse_graph
    , read_file
    ) where

import Insert 
import DataTypes
import GHC.Int

-- | breaks each line into individual elements. e.g from "currency, date, low" to "currency","date","low"
breakLine :: String 
          -> String 
          -> [String] 
          -> [String]
breakLine [] y ys = (reverse y):ys
breakLine (',':xs) y ys = breakLine xs "" ((reverse y):ys)
breakLine (c:xs) y ys = breakLine xs (c:y) ys

-- | from csv to string
read_file :: String 
          -> IO String
read_file currency = readFile $ currency ++ ".csv"

-- | convert elements from string to type currency
convert :: String 
        -> String
        -> CryptoCompare  
convert currency s = CryptoCompare currency d (read o) (read h) (read l) (read c) (read a) (read v)
  where [d,o,h,l,c,a,v] = reverse $ breakLine s "" []

-- | parse through all the lines in the document
parse :: String -> String -> [CryptoCompare]
parse currency s = map (convert currency) (tail $ lines s)

-- | convert line to return user selected data for graph
convert_graph :: String -> String -> String -> (String,[Double])
convert_graph currency g s
 | g == "Volume" = (d, [read v])
 | g == "Open" = (d, [read o])
 | g == "Close" = (d, [read c])
 | g == "High" = (d, [read h])
 | g == "Low" = (d, [read l])
 | g == "Adj" = (d, [read a])
 where [d,o,h,l,c,a,v] = reverse $ breakLine s "" []

-- | parse through lines to get user selected data for graph
parse_graph :: String -> String -> String -> [(String,[Double])]
parse_graph currency g s = map (convert_graph currency g) (tail $ lines s)