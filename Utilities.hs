module Utilities where
import Data.List

--splits string based on char delimiter
splitOn :: Char -> String -> [String]
splitOn c s = map (\a -> [x | x <- a, x /= c]) $ groupBy (\a b -> b /= c) s

--custom implementation of map that takes in a second constant argument
mapp :: (a -> b -> a) -> [a] -> b -> [a]
mapp _ []     ys = []
mapp f (x:xs) ys = f x ys : mapp f xs ys
