{-# LANGUAGE OverloadedStrings #-}


--- This module handles the abstraction needed to represent a Calendar 
module Calendar
( Calendar(Calendar, days, cmonth)
, generateCal
, getMonthName
    )
    where

import qualified Data.Text as T
import           Data.Vector hiding (replicate, (++))
import           Data.Time.Calendar
import           Data.Time.Calendar.WeekDate

data Calendar  = Calendar { cmonth :: T.Text
                          , days :: [[Int]]
                          }  deriving(Show)



listOfMonths :: Vector T.Text
listOfMonths = fromList [ "January", 
                          "February",
                          "March",
                          "April",
                          "May",
                          "June",
                          "July",
                          "August",
                          "September",
                          "October",
                          "November",
                          "December"]
getMonthName :: Int -> T.Text
getMonthName i = listOfMonths ! (i - 1)

generateDays :: Int -> Int -> Int -> [[Int]]
generateDays ctr total w  -- w = week day of the first day of the month | 0 = Monday etc.
    | ctr == total = [total] : []
    | ctr+6 >= total = [x | x <- [ctr.. total]] : []
    | ctr == 1 = (replicate w 0 ++ [x | x <- [ctr..(ctr+(6-w))]] ) : generateDays (ctr+(7-w)) total w 
    | otherwise = [x | x <- [ctr..(ctr+6)]] : generateDays (ctr+7) total w 
    

generateCal :: Integer -> Int -> Calendar
generateCal y m = 
    let numberOfDays = gregorianMonthLength y m
        n = getMonthName m
        (_,_,w) = toWeekDate $ fromGregorian y m 1
        in Calendar n (generateDays 1 numberOfDays (w - 1))






