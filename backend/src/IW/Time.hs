module IW.Time
       ( getToday
       , julianDayToIso
       ) where

import Data.Time (Day (..), getCurrentTime, utctDay)
import Data.Time.Format (formatTime, defaultTimeLocale, iso8601DateFormat)


-- | Returns today's date as a Julian day.
getToday :: IO Day
getToday = utctDay <$> getCurrentTime

-- | Converts a Julian day to a date in ISO 8601 (yyyy-mm-dd) format.
julianDayToIso :: Day -> Text
julianDayToIso = fromString . formatTime defaultTimeLocale (iso8601DateFormat Nothing)
