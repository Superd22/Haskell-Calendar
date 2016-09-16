{-# LANGUAGE OverloadedStrings #-}

--- This module handles the abstractions needed to represent a new event and the digestive functors associated with it.

module Event
(Event
 , Date
 , eventForm
 , Time (Time, hour, minutes)
 , Event (Event, title, date, time, repeat)
 , Date (Date, day, month, year)
    ) where

import qualified Data.Text as T
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap
import           Data.Time.Calendar
import           Control.Applicative

data Time = Time
    {
      hour :: Int
    , minutes:: Int
    } deriving (Show)

data Date = Date 
    {
      day :: Int
    , month :: Int
    , year  :: Integer
    } deriving (Show)



--used only to capture data from form
data Event = Event 
    { 
      title :: T.Text
    , date :: Date
    , time :: Time
    , repeat :: T.Text
} deriving (Show)


timeForm :: Monad m => Form T.Text m Time
timeForm = Time
    <$> "hour" .: stringRead "Hour: Not a number" Nothing
    <*> "minutes" .: stringRead "Minutes: Not a number" Nothing

dateForm :: Monad m => Form T.Text m Date
dateForm = check "Invalid date" checkDate $ Date
    <$> "day"   .: stringRead "Day: Not a number" Nothing
    <*> "month" .: stringRead "Month: Not a number" Nothing
    <*> "year"  .: stringRead "Year: Not a number" Nothing
    where
        checkDate (Date day month year) = 
            day >= 1 && day <= 31 &&
            month >= 1 && month <= 12 &&
            day <= gregorianMonthLength year month

eventForm :: Monad m => Form T.Text m Event
eventForm = Event
    <$> "title"  .: text (Just "Event")
    <*> "date"   .: dateForm
    <*> "time"   .: timeForm 
    <*> "repeat" .: choice [("Once", "Once"),("Daily","Each day"),("Weekly","Each week"),("Monthly", "Each month"),("Yearly", "Each year")] Nothing




