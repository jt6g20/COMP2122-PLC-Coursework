module Utilities where

import Grammar
import Data.List
import Data.Maybe
import Text.Read

type Triple = (String, String, String)

--code from https://codereview.stackexchange.com/a/7009 
--splits string based on char delimiter
splitOn :: Char -> String -> [String]
splitOn c s = map (\a -> [x | x <- a, x /= c]) $ groupBy (\a b -> b /= c) s

--custom implementation of map that takes in a second constant argument
mapp :: (a -> b -> a) -> [a] -> b -> [a]
mapp _ []     ys = []
mapp f (x:xs) ys = f x ys : mapp f xs ys

--gets specified attribute from triple, and just returns constant as is
getAtt :: Attribute -> Triple -> String
getAtt Subj (x, _, _) = x
getAtt Pred (_, x, _) = x
getAtt (AttributeObj Obj) (_, _, x) | "+" `isPrefixOf` x = tail x
      | otherwise = x
getAtt (AttributeObj (ObjAdd i)) (_, _, x) | isNothing num = ":("
                                           | otherwise = show (fromJust num + i)
    where num = readAdd x
          readAdd ('+':xs) = readMaybe xs
          readAdd xs = readMaybe xs
getAtt (AttributeString s) x = s
getAtt (AttributeBoolean True) x = "true" --ttl uses lowercase for booleans
getAtt (AttributeBoolean False) x = "false"
getAtt _ _ = error "invalid attribute" --Attributes constructor not allowed

--concats elements and adds spaces depending on if they are URIs 
join :: [String] -> String
join [x,y,z] | "<" `isPrefixOf` z || "\"" `isPrefixOf` z = x ++ y ++ z
             | otherwise = x ++ y ++ " " ++ z
join [x,y] | "<" `isPrefixOf` y  || "\"" `isPrefixOf` y= x ++ y
           | otherwise = x ++ " " ++ y
join [x] = x
join _ = error "out of scope"