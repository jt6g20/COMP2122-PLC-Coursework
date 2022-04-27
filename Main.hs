module Main where

import Tokens
import System.IO
import Data.List
import Data.Char
import Data.Maybe
import Text.Read
import Grammar
import System.Environment
import Control.Exception
import Utilities
import PredicateLists
import ObjectLists
import Bases
import Prefixes
import SelectAttributes
import SortOut

main :: IO ()
-- main = catch lexer handler
main = do
    (fileName : _) <- getArgs
    stmtString <- readFile fileName

    let stmts = map (parseSQL . alexScanTokens) (lines stmtString)
    
    triples <- getTriples stmts

    -- lists of statements and, if specified, inner statement
    let stmtPairs = [stmtToInnerPair x | x <- stmts]

    triplePairs <- mapM getTriples stmtPairs

    putStrLn $ sortOut $ evalIt stmts triplePairs

    if isOutFile (last stmts) then 
        writeFile (getOutFile (last stmts)) (sortOut (evalIt stmts triplePairs)) 
        else print (sortOut (evalIt stmts triplePairs))


--takes a list of statements and returns and IO object of a list of lists of their triples
getTriples :: [Stmt] -> IO [[Triple]]
getTriples stmts = do
    let inputFiles = map queryFile stmts
    contents <- mapM (mapM readFile) inputFiles

    let triples = map inputsToTriples contents
    return triples

--gets the inner statement
getInStmt :: Stmt -> Maybe Stmt
getInStmt (Stmt (QueryCondition _ _ (AttributeIn _ q))) = Just $ Stmt q
getInStmt (StmtOutput (QueryCondition _ _ (AttributeIn _ q)) _) = Just $ Stmt q
getInStmt _ = Nothing

--takes a statement and returns a list of that statement and, if there, it's IN statement
stmtToInnerPair :: Stmt -> [Stmt]
stmtToInnerPair s | isNothing (getInStmt s) = [s]
                  | otherwise = [s, fromJust $ getInStmt s]

queryFile :: Stmt -> [FilePath]
queryFile (Stmt (Query _ f)) = getFilePaths f
queryFile (StmtOutput (Query _ f) _) = getFilePaths f
queryFile (Stmt (QueryCondition _ f _)) = getFilePaths f
queryFile (StmtOutput (QueryCondition _ f _) _) = getFilePaths f

--Converts Files to list of FilePaths in Inputs folder
getFilePaths :: File -> [FilePath]
getFilePaths (File x) = ["Inputs/" ++ x ++ ".ttl"]
getFilePaths (Files x y) = getFilePaths x ++ getFilePaths y

isOutFile :: Stmt -> Bool
isOutFile (StmtOutput _ _) = True
isOutFile _ = False

getOutFile :: Stmt -> String
getOutFile (StmtOutput (Query {}) s) = s ++ ".ttl"
getOutFile (StmtOutput (QueryCondition {}) s) = s ++ ".ttl"

inputsToTriples :: [String] -> [Triple]
inputsToTriples = foldr
      (\ x
         -> (++)
              (stringListToTripleList
                 (onlyTriples
                    $ prefixes $ bases $ objLists $ predLists $ inputToList x)))
      []

onlyTriples :: [String] -> [String]
onlyTriples xs = [a:as | (a:as) <- xs, a /= '@']

inputToList :: String -> [String]
inputToList s = [replace x | x <- lines s, x /= ""]

replace :: String -> String
replace xs | Just xs <- stripPrefix "><" xs = "> <" ++ replace xs
replace (x:xs) = x : replace xs
replace [] = []

tripleListToTriple :: [String] -> Triple
tripleListToTriple xs = (head xs, xs!!1, xs!!2)

stringListToTripleList :: [String] -> [Triple]
stringListToTripleList = map (tripleListToTriple . words)

condition :: String -> [Triple] -> [Triple]
condition s xs = [x | x <- xs, subjMatch s x]

subjMatch :: String -> Triple -> Bool
subjMatch s (x, _, _) = x == s
predMatch :: String -> Triple -> Bool
predMatch s (_, x, _) = x == s
objMatch :: String -> Triple -> Bool
objMatch s (_, _, x) = x == s

--takes statments and their triples in two lists
evalIt :: [Stmt] -> [[[Triple]]] -> [[String]]
evalIt [] _ = []
evalIt _ [] = []
evalIt (x:xs) (y:ys) = evaluator x y ++ evalIt xs ys

--takes a statement and the triples in it's file and applies select and conditions
evaluator :: Stmt -> [[Triple]] -> [[String]]
evaluator (Stmt q) ts = handleQuery q ts
evaluator (StmtOutput q s) ts = handleQuery q ts

join :: [String] -> String
join [x,y,z] | "<" `isPrefixOf` z = x ++ y ++ z
             | otherwise = x ++ y ++ " " ++ z
join [x,y] | "<" `isPrefixOf` y = x ++ y
           | otherwise = x ++ " " ++ y
join _ = error "out of scope"

handleQuery :: Query -> [[Triple]] -> [[String]]
handleQuery (QueryCondition a f c) ts = select a (rule c ts)
handleQuery (Query a f) ts = select a (head ts)

--WHERE clause: uses list comprehension to filter triples with given conditions 
rule :: Condition -> [[Triple]] -> [Triple]
rule (ConditionAND x y) ts = rule x ts `intersect` rule y ts
rule (ConditionOR x y) ts = rule x ts `union` rule y ts

--checks whether object attribute is integer before comparison
rule (Greater x@(AttributeObj Obj) n) ts = [t | t <- head ts, (readMaybe (getAtt x t) :: Maybe Int) > Just n]
--only Obj can be Int
rule (Greater x n) ts = error ("invalid attribute " ++ show x ++ " to compare '>' with " ++ show n)

--slightly different implementation to Greater as Maybe type declares Nothing < Just n = True
rule (Less x@(AttributeObj Obj) n) ts = [t | t <- head ts, less (readMaybe (getAtt x t) :: Maybe Int) n]
    where less x n | isJust x = x < Just n
                   | otherwise = False
rule (Less x n) ts = error ("invalid attribute " ++ show x ++ " to compare '<' with " ++ show n)

rule (NumEq x@(AttributeObj Obj) n) ts = [t | t <- head ts, (readMaybe (getAtt x t) :: Maybe Int) == Just n]
rule (NumEq x n) ts = error ("invalid attribute " ++ show x ++" to compare '=' with " ++ show n)

rule (AttributeEq x y) ts = [t | t <- head ts, getAtt x t == getAtt y t]

rule (AttributeIn x y) ts = [t | t <- head ts, getAtt x t `elem` concat (evaluator (Stmt y) [ts!!1])]