module SortOut where

import Data.List
import Data.Function
import Data.Maybe

sortOut :: [[String]] -> String
sortOut xss = concatMap (\x -> join x ++ " .\n") (sortAtt (sortBy (compare `on` head) (rmvDupl xss)) (-1))

join :: [String] -> String
join [x,y,z] | "<" `isPrefixOf` z = x ++ y ++ z
             | otherwise = x ++ y ++ " " ++ z
join [x,y] | "<" `isPrefixOf` y = x ++ y
           | otherwise = x ++ " " ++ y
join _ = error "out of scope"

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

rmvDupl :: Eq a => [[a]] -> [[a]]
rmvDupl [] = []
rmvDupl (xs:xss) | (allDupl 0 xs xss) = rmvDupl xss
                 | otherwise = xs : rmvDupl xss

allDupl :: Eq a => Int -> [a] -> [[a]] -> Bool
allDupl n [] xss = True
allDupl n (x:xs) xss = x `elem` (map (!!n) xss) && (allDupl (n+1) xs xss)