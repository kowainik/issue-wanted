module IW.Time
       ( julianDayToIso
       ) where

import Data.Time (Day)
import Data.Time.Format (formatTime, defaultTimeLocale, iso8601DateFormat)


julianDayToIso :: Day -> Text
julianDayToIso = fromString . formatTime defaultTimeLocale (iso8601DateFormat Nothing)
