module IW.Time
       ( firstHaskellRepoCreated
       , getToday
       , julianDayToIso
       ) where

import Data.Time (Day (..), getCurrentTime, utctDay)
import Data.Time.Format (formatTime, defaultTimeLocale, iso8601DateFormat)


firstHaskellRepoCreated :: Day
firstHaskellRepoCreated = ModifiedJulianDay 58211

getToday :: IO Day
getToday = getCurrentTime >>= pure . utctDay

julianDayToIso :: Day -> Text
julianDayToIso = fromString . formatTime defaultTimeLocale (iso8601DateFormat Nothing)
