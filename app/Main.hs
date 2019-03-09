{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parse
import Insert
import Retrieve
import Crypto
import Delete
import AveragePrice
import System.Environment
import Database.SQLite3
import GHC.Int
import Data.ByteString.Lazy.Char8 as S
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
-- | this will ask user to input a 'cia' from a list of 'cias' and feed the cia into functions to store information
processCia :: Database 
           -> [String] -- ^ list of 'currencies'
           -> IO()   
processCia db cias = do
    Prelude.putStrLn $ "Which data are you interested in? " ++ show(cias)
    cia <- getLine
    Prelude.putStrLn $ "Select how you wish to view or if you would like to delete? [Database] [Graph] [Delete]"
    info <- getLine
    if cia `Prelude.elem` cias && info=="Database" then
        do
            print $ "-> Processing " ++ cia
            response <- getCrypto cia
            let cia2 = Prelude.drop 4 $ cia
            let bitcoin = parse cia2 (unpack response)
            batchInsert cia2 bitcoin
            Prelude.putStrLn $ "Database created. Check current folder."
    else if cia `Prelude.elem` cias && info=="Delete" then
        do
            Prelude.putStrLn $ "Do you want to delete the whole database or a particular entry? [Database] [Entry]"
            response <- getLine
            if response == "Database" then 
               do
                 deleteData db (Prelude.drop 4 $ cia)
            else if response == "Entry" then 
               do
                  Prelude.putStrLn $ "What date do you want to delete the data for? [YYYY-MM-DD]"
                  date <- getLine
                  deleteForDate db date
            else
               Prelude.putStrLn $ "You chose to not delete data."
    else if cia `Prelude.elem` cias && info=="Graph" then
        do
            Prelude.putStrLn $ "Select graph to create? [Volume] [Open] [Close] [High] [Low] [Adj]"
            info <- getLine
            if (info `Prelude.elem` ["Volume", "Adj", "Open", "Close", "High", "Low"]) then do
                let graph_titles = [info]
                response <- getCrypto cia
                let graph_values = parse_graph cia info (unpack response)
                let graph_name = info
                let filename = cia ++ graph_name ++ "_graph.png"
                toFile def filename $ do
                    layout_title .= graph_name ++ " graph for " ++ cia
                    layout_title_style . font_size .= 10
                    layout_x_axis . laxis_generate .= autoIndexAxis (Prelude.map fst graph_values)
                    plot $ fmap plotBars $ bars graph_titles (addIndexes (Prelude.map snd graph_values))
            else
                Prelude.putStrLn "The input you entered is invalid."

    else
        Prelude.putStrLn $ "Please pick from the list above!"

-- | this will ask user to input a 'currency' from a list of 'currencies' and feed the result into functions to query data from database
query :: Database 
           -> [String] -- ^ list of 'currencies'
           -> IO()  
query db cias = do
    Prelude.putStrLn $ "Select currency to query " ++ show(cias)
    cia <- getLine
    if cia `Prelude.elem` cias then
        do
            let cia2 = Prelude.drop 4 $ cia
            response <- getCrypto cia
            let bitcoin = parse cia2 (unpack response)
            batchInsert cia2 bitcoin
            averageClose <- averageClose db cia2
            averageOpen <- averageOpen db cia2
            Prelude.putStrLn $ "Average close price for " ++ cia ++ " is " ++ (show averageClose)
            Prelude.putStrLn $ "Average open price for " ++ cia ++ " is " ++ (show averageOpen)
            Prelude.putStrLn $ "Select a date to view the closing price for. (2017-11-10 to 2017-12-10)"
            date <- getLine
            currencyInf <- currencyInfo db date
            Prelude.putStrLn $ "Currency details: " ++ (show currencyInf)
            openVal <- openForDate db date
            closeVal <- closeForDate db date
            Prelude.putStrLn $ "On " ++ date ++ " " ++ cia ++ " opened at " ++ (show openVal) ++ ", closed at " ++ (show closeVal)
            let yearCheck = read (Prelude.take 4 date)
            let monthCheck = read (Prelude.take 2 (Prelude.drop 5 date))
            let dayCheck = read (Prelude.take 2 (Prelude.drop 8 date))
            let diff = closeVal - openVal
            if (yearCheck == 2017) && (monthCheck `Prelude.elem` [1..12]) && (dayCheck `Prelude.elem` [1..30]) then if diff > 0.0 then 
                 Prelude.putStrLn $ cia ++ " increased by a total of " ++ (show diff) ++ " on " ++ date
                else if diff < 0.0 then
                 Prelude.putStrLn $ cia ++ " decreased by a total of "  ++ (show diff) ++ " on " ++ date
                else
                 Prelude.putStrLn $ "There was no data for this date."
            else 
              Prelude.putStrLn $ "Date entered was invalid. YYYY must be 2017, MM must be 1-12, DD must be 1-30."
    else 
        Prelude.putStrLn $ " Invalid currency entered."
    {-Prelude.putStrLn $ "Do you want to run another query?"
    response <- getLine
    if response == "Y" || response == "Yes" || response == "yes" then 
       query db cias
    else
       Prelude.putStrLn $ "You chose to stop querying. Quitting application..."-}

-- | main function for application which performs IO
main :: IO ()
main = do
    db <- database
    initialiseDB db
    let cias = ["BTC-GBP", "BTC-EUR", "BTC-USD"]
    processCia db cias
    query db cias
