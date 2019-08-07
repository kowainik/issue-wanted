module IW.Time
       ( firstHaskellRepoCreated
       , getToday
       , julianDayToIso
       ) where

import Data.Time (Day (..), getCurrentTime, utctDay)
import Data.Time.Format (formatTime, defaultTimeLocale, iso8601DateFormat)


-- | This day is equilvalent to 2008-04-03.
firstHaskellRepoCreated :: Day
firstHaskellRepoCreated = ModifiedJulianDay 54559

getToday :: IO Day
getToday = utctDay <$> getCurrentTime

-- | Converts a Julian day to a date in ISO 8601 (yyyy-mm-dd) format.
julianDayToIso :: Day -> Text
julianDayToIso = fromString . formatTime defaultTimeLocale (iso8601DateFormat Nothing)
