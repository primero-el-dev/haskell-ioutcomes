module Text where

import Text.Printf
import System.IO


printfRecursive :: String -> [String] -> String
printfRecursive template [] = template
printfRecursive template (x:xs) =
    printfRecursive (printf template x :: String) xs


printfFile :: FilePath -> [String] -> IO String
printfFile path args = do
    handle <- openFile path ReadMode
    content <- System.IO.hGetContents handle
    return (printfRecursive content args)