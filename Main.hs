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
    (fileName : _) <- getArgs
    stmtString <- readFile fileName
    let stmt = parseSQL $ alexScanTokens stmtString
    let fileName = queryFile stmt

    contents <- readFile ("Inputs/" ++ fileName ++ ".ttl")
    let triples = stringListToTripleList $ onlyTriples $ prefixes $ bases $ objLists $ predLists $ inputToList contents
    putStrLn $ show $ triples


-- Stmt (QueryCondition (Attributes Subj (Attributes Pred (AttributeObj Obj))) (File "foo") (ConditionOR (AttributeEq Pred (AttributeString "http://www.cw.org/problem3/#predicate1")) (ConditionOR (AttributeEq Pred (AttributeString 
-- "http://www.cw.org/problem3/#predicate2")) (AttributeEq Pred (AttributeString "http://www.cw.org/problem3/#predicate3")))))

queryFile :: Stmt -> String
queryFile (Stmt (Query _ (File f))) = f
queryFile (StmtOutput (Query _ (File f)) _) = f
queryFile (Stmt (QueryCondition _ (File f) _)) = f
queryFile (StmtOutput (QueryCondition _ (File f) _) _) = f

subj :: Triple -> String
subj (x, _, _) = x
pred :: Triple -> String
pred (_, x, _) = x
obj :: Triple -> String
obj (_, _, x) = x

-- lexer :: IO ()
-- lexer = do
--     (fileName : _) <- getArgs
--     contents <- readFile fileName
--     -- putStrLn $ show $ parseSQL $ alexScanTokens contents
--     putStrLn $ show $ parseSQL $ alexScanTokens contents

-- handler :: ErrorCall -> IO ()
-- handler e = do
--     let error = show e
--     putStrLn ("Error " ++ error)

onlyTriples :: [String] -> [String]
onlyTriples xs = [(a:as) | (a:as) <- xs, a /= '@']

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


type Triple = (String, String, String)

tripleListToTriple :: [String] -> Triple
tripleListToTriple xs = (xs!!0, xs!!1, xs!!2)

stringListToTripleList :: [String] -> [Triple]
stringListToTripleList = map (tripleListToTriple . words)

condition :: String -> [Triple] -> [Triple]
condition s xs = [x | x <- xs, subjMatch s x]

subjMatch :: String -> Triple -> Bool
subjMatch s (x, _, _) | x == s = True
                      | otherwise = False
predMatch :: String -> Triple -> Bool
predMatch s (_, x, _) | x == s = True
                      | otherwise = False
objMatch :: String -> Triple -> Bool
objMatch s (_, _, x) | x == s = True
                     | otherwise = False

