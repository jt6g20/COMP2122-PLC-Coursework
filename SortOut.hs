module SortOut where

import Utilities
import Data.List
import Data.Function
import Data.Maybe

--code from https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell-without-elem
--removes duplicates and sorts the output before transforming it to the final output
sortOut :: [[String]] -> String
sortOut xss = concatMap (\x -> join x ++ " .\n") (sortAtt (sortBy (compare `on` head) (rmvDupl xss)) (-1))

--calls the sorting function with regards to each attribute in a row
sortAtt :: Ord a => [[a]] -> Int -> [[a]]
sortAtt xss@(xs:_) n | n == length xs - 1 = xss
                     | otherwise = sortAtt (sortOnAtt n xss) (n+1)

--sorts a list based on the specified index, without affecting the sort of the elements before the index
--concats sublists with the matching elements up to the index
sortOnAtt :: Ord a => Int -> [[a]] -> [[a]]
sortOnAtt _ [] = []
sortOnAtt n xss@(xs:_) | n == length xs - 1 = xss
                       | otherwise = takeOn ++ sortOnAtt n (drop (length takeOn) xss)
    where takeOn = takeOnAtt n xss

--takes the max amount of elements from matching lists up to the specified index and sorts them
--this is used when sorting predicates and objects without affecting the sort of the atributes before them
takeOnAtt :: Ord a => Int -> [[a]] -> [[a]]
takeOnAtt n (xs:xss) = sortBy (compare `on` (!!(n+1))) (xs : takeWhile (\x -> take (n+1) x == take (n+1) xs) xss)

--removes duplicates from a list of lists
rmvDupl :: Eq a => [[a]] -> [[a]]
rmvDupl [] = []
rmvDupl (xs:xss) | xs `elem` xss = rmvDupl xss
                 | otherwise = xs : rmvDupl xss