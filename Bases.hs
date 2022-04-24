module Bases where
import Utilities
import Data.List
import Data.Char

bases :: [String] -> [String]
bases xs = [ concatMap (addBase (getBase xs)) (splitTriple x) | x <- xs, ("<" `isPrefixOf` x)]

splitTriple :: String -> [String]
splitTriple xs = concatMap (splitOn '<') $ splitOn '>' $ filter (not . (`elem` " ")) xs

getBase :: [String] -> String
getBase (('@':'b':'a':'s':'e':' ':x):xs) = tail(takeWhile (/='>') x)
getBase (x:xs) = getBase xs
getBase [] = ""

addBase :: String -> String -> String
addBase base s | "http" `isPrefixOf` s = "<" ++ removeDot s ++ ">"
    | ((s /= "true") && (s /= "false") && isAlpha (head s)) || "#" `isPrefixOf` s = "<" ++ base ++ removeDot s ++ ">"
    | otherwise = " " ++ removeDot s

removeDot :: String -> String
removeDot xs | last xs == '.' = init xs
    | otherwise = xs