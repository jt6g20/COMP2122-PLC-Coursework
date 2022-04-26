module Prefixes where
import Utilities
import Data.List

--builds the list of prefixes from the whole string to refer to when rewriting
--and calls the actual iterative function
prefixes :: [String] -> [String]
prefixes x = prefixIt x (buildPrefixes x)

--maps the rewriting function to each subj, pred, obj for lines not starting with @
prefixIt :: [String] -> [(String, String)] -> [String]
prefixIt [] ps = []
prefixIt (a@('@':'p':x):xs) ps = a : prefixIt xs ps
prefixIt (x:xs) ps = unwords (mapp rmvPrefix (take 3 (splitOn ' ' x)) ps) : prefixIt xs ps

--if string isn't already URI or base URI reference,
--find the prefix reference and return the new string
rmvPrefix :: String -> [(String, String)] -> String
rmvPrefix x ps | not ("<" `isPrefixOf` x) && (":" `isInfixOf` x) = findURI (head split) ps ++ last split ++ ">"
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
                           uri = init (splitOn ' ' x !! 2)