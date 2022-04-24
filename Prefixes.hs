module Prefixes where
import Utilities
import Data.List

--builds the list of prefixes from the whole string to refer to when rewriting
--and calls the actual iterative function
prefixes :: [String] -> [String]
prefixes x = prefixIt x (buildPrefixes x)

--maps the rewriting function to each subj, pred, obj for lines not starting with @
--prefixIt :: [String] -> [(String, String)] -> [String]
--prefixIt x ps = ps
prefixIt [] ps = []
prefixIt (x:xs) ps | "@" `isPrefixOf` x = x : prefixIt xs ps
                   | otherwise = (unwords (mapp rmvPrefix (take 3 (splitOn ' ' x)) ps){- ++ " ."-}) : prefixIt xs ps

--custom implementation of map that takes in a second constant argument
mapp :: (a -> b -> a) -> [a] -> b -> [a]
mapp _ []     ys = []
mapp f (x:xs) ys = f x ys : mapp f xs ys

--if string isn't already URI or base URI reference,
--find the prefix reference and return the new string
rmvPrefix :: String -> [(String, String)] -> String
rmvPrefix x ps | not ("<" `isPrefixOf` x) && (":" `isInfixOf` x) = "<" ++ findURI (head split) ps ++ last split ++ ">"
               | otherwise = x
               where split = splitOn ':' x
                     findURI x [] = error "unknown prefix"
                     findURI x (p:ps) | x == fst p = snd p
                                      | otherwise = findURI x ps

--builds a list of prefix and URI pairs from @prefix lines
buildPrefixes :: [String] -> [(String, String)]
buildPrefixes [] = []
buildPrefixes (x:xs) | "@prefix" `isPrefixOf` x = (name, uri) : buildPrefixes xs
                     | otherwise = buildPrefixes xs
                     where name = init (splitOn ' ' x !! 1)
                           uri = init (tail (splitOn ' ' x !! 2))