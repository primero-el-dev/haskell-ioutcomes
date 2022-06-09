module DateTime where

import Data.Time.Clock
import Data.DateTime


dateFromNow :: NominalDiffTime -> NominalDiffTime -> IO DateTime
dateFromNow multiplier value = do
    dateTime <- Data.Time.Clock.getCurrentTime
    return (addUTCTime (multiplier * value) dateTime)


dateMinutesFromNow :: NominalDiffTime -> IO DateTime
dateMinutesFromNow = dateFromNow 60
