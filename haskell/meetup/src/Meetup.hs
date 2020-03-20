module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, fromGregorian, toGregorian, gregorianMonthLength)

data Weekday = Sunday
             | Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday deriving (Show, Enum, Eq)

data Schedule = First
              | Second
              | Third
              | Fourth
              | Last
              | Teenth deriving (Show, Enum)

-- Tøndering algorithm from https://tondering.dk/claus/cal/chrweek.php.
toWeekday :: Int -> Int -> Int -> Weekday
toWeekday year month day = let a = (14 - month) `quot` 12
                               y = year - a
                               m = month + 12 * a - 2 in
                             toEnum $ (day + y + (y `quot` 4) - (y `quot` 100) + (y `quot` 400) + ((31 * m) `quot` 12)) `mod` 7


meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = select
                                        $ filter filterWeekday
                                        $ map toGregorianAndWeekday [1..(gregorianMonthLength year month)]
  where toGregorianAndWeekday day = (fromGregorian year month day, toWeekday (fromIntegral year) month day)
        filterWeekday (_, day) = day == weekday
        select days = case schedule of
          First -> fst $ head days
          Second -> fst $ head $ drop 1 days
          Third -> fst $ head $ drop 2 days
          Fourth -> fst $ head $ drop 3 days
          Last -> fst $ last days
          Teenth -> fst $ head $ filter (\(date, _) -> let (_, _, dayIdx) = toGregorian date in dayIdx `elem` [13..19]) days
