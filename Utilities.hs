module Utilities where
import Data.List

splitOn :: Char -> String -> [String]
splitOn c s = map (\a -> [x | x <- a, x /= c]) $ groupBy (\a b -> b /= c) s
