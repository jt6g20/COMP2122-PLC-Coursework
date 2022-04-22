import Tokens
import System.IO
import Data.List
import Grammar
import System.Environment
import Control.Exception

main :: IO ()
-- main = catch lexer handler
main = do
    contents <- readFile "Inputs/foo.ttl"
    fileName <- getLine
    putStrLn $ listToOutput $ replacePredList $ inputToList contents

-- lexer :: IO ()
-- lexer = do
--     (fileName : _) <- getArgs
--     contents <- readFile fileName
--     putStrLn $ show $ parseSQL $ alexScanTokens contents

-- handler :: ErrorCall -> IO ()
-- handler e = do
--     let error = show e
--     putStrLn ("Error " ++ error)

inputToList :: String -> [String]
inputToList s = [x | x <- lines $ s, x /= ""]

listToOutput :: [String] -> String
listToOutput [] = []
listToOutput (x:xs) = x ++ "\n" ++ listToOutput xs


predListEvaluator :: String -> [String] -> [String]
predListEvaluator "" (x:xs) = x : predListEvaluator (head $ words $ x) xs
predListEvaluator subj (x:xs) = (subj ++ x) : predListEvaluator subj xs
predListEvaluator _ _ = []

predListSeparator :: String -> [String]
predListSeparator s = map (\a -> [x | x <- a, x /= ';']) $ groupBy (\a b -> b /= ';') s

predicateList :: String -> [String]
predicateList s = predListEvaluator "" $ predListSeparator s

replacePredList :: [String] -> [String]
replacePredList (x:xs) | ';' `elem` x = predicateList x ++ replacePredList xs
                       | otherwise = x : replacePredList xs
replacePredList _ = []


-- "<testSubA> <testPredList> -5 ; <testPredList> 10 ; <testPredList> 20 ."

-- type Triple = (String, String, String)

-- stringToTripleList :: String -> [String]
-- stringToTripleList s = words s

-- tripleListToTriple :: [String] -> Triple
-- tripleListToTriple xs = (xs!!0, xs!!1, xs!!2)