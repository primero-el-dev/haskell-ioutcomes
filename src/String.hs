module String where

import System.Random


generateRandomString :: [Char] -> Int -> IO String
generateRandomString [] _ = return ""
generateRandomString _ 0 = return ""
generateRandomString chars len = do
    g <- newStdGen
    (index, _) <- return (randomR (0, length chars - 1) g)
    (chars !! index :) <$> generateRandomString chars (len - 1)


generateRandomAlphanumString :: Int -> IO String
generateRandomAlphanumString = generateRandomString "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"