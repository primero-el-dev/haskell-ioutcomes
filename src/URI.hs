module URI where

import Data.List.Split


data URIType = Absolute | RequestURI

data URI = URI URIType String

data Param = String | Int


splitAtFirst :: Eq a => a -> [a] -> ([a], [a])
splitAtFirst x = fmap (drop 1) . break (x ==)


getRequestParams :: [(String, Param)] -> String -> [(String, Param)]
getRequestParams accum path =
    splitAtFirst "/" path


--class Get

--splitOn "x"