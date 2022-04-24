import Tokens
import System.IO
import Data.List
import Data.Char
import Grammar
import System.Environment
import Control.Exception
import Utilities
import PredicateLists
import ObjectLists
import Bases
import Prefixes

main :: IO ()
-- main = catch lexer handler
main = do
    contents <- readFile "Inputs/foo.ttl"
    putStrLn $ listToOutput $ prefixes $ bases $ objLists $ predLists $ inputToList contents

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
inputToList s = [replace x | x <- lines s, x /= ""]

listToOutput :: [String] -> String
listToOutput [] = []
listToOutput (('@':x):xs) = listToOutput xs
listToOutput (x:xs) = x ++ " .\n" ++ listToOutput xs

replace :: String -> String
replace xs | Just xs <- stripPrefix "><" xs = "> <" ++ replace xs
replace (x:xs) = x : replace xs
replace [] = []


-- "<testSubA> <testPredList> -5 ; <testPredList> 10 ; <testPredList> 20 ."

-- type Triple = (String, String, String)

-- stringToTripleList :: String -> [String]
-- stringToTripleList s = words s

-- tripleListToTriple :: [String] -> Triple
-- tripleListToTriple xs = (xs!!0, xs!!1, xs!!2)