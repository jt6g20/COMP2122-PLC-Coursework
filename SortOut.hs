module SortOut where

import Utilities
import Data.List
import Data.Function
import Data.Maybe

sortOut :: [[String]] -> String
sortOut xss = concatMap (\x -> join x ++ " .\n") (sortAtt (sortBy (compare `on` head) (rmvDupl xss)) (-1))

sortAtt :: Ord a => [[a]] -> Int -> [[a]]
sortAtt xss@(xs:_) n | n == length xs - 1 = xss
                     | otherwise = sortAtt (sortOnAtt n xss) (n+1)

sortOnAtt :: Ord a => Int -> [[a]] -> [[a]]
sortOnAtt _ [] = []
sortOnAtt n xss@(xs:_) | n == length xs - 1 = xss
                       | otherwise = takeOn ++ sortOnAtt n (drop (length takeOn) xss)
    where takeOn = takeOnAtt n xss

takeOnAtt :: Ord a => Int -> [[a]] -> [[a]]
takeOnAtt n (xs:xss) = sortBy (compare `on` (!!(n+1))) (xs : takeWhile (\x -> take (n+1) x == take (n+1) xs) xss)

rmvDupl :: [[String]] -> [[String]]
rmvDupl [] = []
rmvDupl (xs:xss) | xs `elem` xss = rmvDupl xss
                 | otherwise = xs : rmvDupl xss