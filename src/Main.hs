module Main where

import Data.Time
import Data.Time.Calendar.WeekDate
import Control.Monad

data Work = Work { startWork :: LocalTime, endWork :: LocalTime}

fajrant :: LocalTime -> (LocalTime, LocalTime)
fajrant localTime = nextInterruption (toWeekDate (localDay localTime)) (localTimeOfDay localTime)
  where
    nextInterruption :: (Integer, Int, Int) -> TimeOfDay -> (LocalTime, LocalTime)
    nextInterruption (year, week, dayOfWeek) (TimeOfDay h m _)
      -- mon - fri before standup
      | dayOfWeek <= 5 && h < 10 = (LocalTime (fromWeekDate year week dayOfWeek) (TimeOfDay 10 0 (read "0")), LocalTime (fromWeekDate year week dayOfWeek) (TimeOfDay 10 30 (read "0")))
      -- mon - fri before lunch
      | dayOfWeek <= 5 && h < 11 = (LocalTime (fromWeekDate year week dayOfWeek) (TimeOfDay 11 0 (read "0")), LocalTime (fromWeekDate year week dayOfWeek) (TimeOfDay 11 30 (read "0")))
      -- fri before demo
      | dayOfWeek == 5 && h < 12 = (LocalTime (fromWeekDate year week dayOfWeek) (TimeOfDay 12 0 (read "0")), LocalTime (fromWeekDate year week dayOfWeek) (TimeOfDay 13 0 (read "0")))
      -- every second fri before retro
      | dayOfWeek == 5 && h < 13 && even week = (LocalTime (fromWeekDate year week dayOfWeek) (TimeOfDay 13 0 (read "0")), LocalTime (fromWeekDate year week dayOfWeek) (TimeOfDay 14 0 (read "0")))
      -- every second tue before technical wednesday
      | dayOfWeek == 2 && h < 14 && even week = (LocalTime (fromWeekDate year week dayOfWeek) (TimeOfDay 14 0 (read "0")), LocalTime (fromWeekDate year week dayOfWeek) (TimeOfDay 15 0 (read "0")))
      -- mon-thu
      | dayOfWeek < 5 = (LocalTime (fromWeekDate year week dayOfWeek) (TimeOfDay 17 30 (read "0")), LocalTime (fromWeekDate year week (dayOfWeek + 1)) (TimeOfDay 9 0 (read "0")))
      -- fri
      | dayOfWeek == 5 = (LocalTime (fromWeekDate year week dayOfWeek) (TimeOfDay 17 30 (read "0")), LocalTime (fromWeekDate year (week + 1) 1) (TimeOfDay 9 0 (read "0")))
      | otherwise = undefined

workSeconds :: Work -> NominalDiffTime
workSeconds (Work start end) =
  let (leaveWork, returnToWork) = fajrant start
  in if leaveWork > end then diffUTCTime (localTimeToUTC utc end) (localTimeToUTC utc start) else diffUTCTime (localTimeToUTC utc leaveWork) (localTimeToUTC utc start) + workSeconds (Work returnToWork end)

newtype Story = Story { work :: [Work] }

workingHours :: [Work] -> Int
workingHours works = ceiling $ sum (fmap workSeconds works) / 3600

imped :: String -> [Work] -> Int -> (String, Int, Int)
imped story works waste = (story, workingHours works, waste)

date :: Int -> Int -> Integer -> Int -> Int -> LocalTime
date day month year hour minute = LocalTime (fromGregorian year month day) (TimeOfDay hour minute (read "0"))

till :: LocalTime -> LocalTime -> Work
infixl 3 `till`
till = Work

stories = [ imped "CTT-1102" [date 30 1 18 14 53 `till` date 12 2 18 9 47] 0,
    imped "CTT-1103" [date 1 2 18 16 37 `till` date 12 2 18 10 6] 0,
    imped "CTT-1120" [date 23 1 18 14 56 `till` date 29 1 18 10 00] 0,
    imped "CTT-1128" [date 30 1 18 16 34 `till` date 31 1 18 16 02] 4,
    imped "CTT-1142" [date 14 2 18 09 27 `till` date 16 2 18 16 17] 3,
    imped "CTT-1144" [date 16 2 18 16 06 `till` date 16 2 18 16 25] 0,
    imped "CTT-1160" [date 08 3 18 13 47 `till` date 09 3 18 11 02] 2,
    imped "CTT-1132" [date 02 2 18 10 14 `till` date 01 3 18 10 03] 0,
    imped "CTT-1143" [date 27 2 18 11 26 `till` date 28 2 18 16 08] 0,
    imped "CTT-1145" [date 21 2 18 14 00 `till` date 28 2 18 16 08] 0,
    imped "CTT-1147" [date 16 2 18 16 28 `till` date 23 2 18 10 24] 3,
    imped "CTT-1153" [date 06 3 18 11 33 `till` date 16 3 18 13 13] 17,
    imped "CTT-1134" [date 07 3 18 14 22 `till` date 15 3 18 15 42] 5,
    imped "CTT-1155" [date 15 3 18 13 07 `till` date 22 3 18 16 16] 1,
    imped "CTT-1140" [date 07 3 18 15 34 `till` date 23 3 18 11 50] 13,
    imped "CTT-1161" [date 21 3 18 09 28 `till` date 22 3 18 16 00] 3,
    imped "CTT-1167" [date 22 3 18 16 33 `till` date 18 4 18 12 51] 6,
    imped "CTT-1165" [date 09 4 18 12 05 `till` date 11 4 18 16 05] 8,
    imped "CTT-1154" [date 23 3 18 13 29 `till` date 06 4 18 10 03] 14,
    imped "CTT-1163" [date 09 4 18 10 12 `till` date 10 4 18 09 56] 2,
    imped "CTT-1129" [date 22 2 18 12 15 `till` date 05 3 18 15 58] 8]

main = let
    (works, wastes) = foldr (\(_, work, waste) (works, wastes) -> (works + work, wastes + waste)) (0, 0) stories
  in print $ fromIntegral wastes / fromIntegral works
