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
main = do
    (fileName : _) <- getArgs
    stmtString <- readFile fileName

    let stmts = map (parseSQL . alexScanTokens) (lines stmtString)
    
    triples <- getTriples stmts

    -- lists of statements and, if specified, inner statement
    let stmtPairs = [stmtToInnerPair x | x <- stmts]

    triplePairs <- mapM getTriples stmtPairs

    putStrLn (sortOut (evalIt stmts triplePairs))
<<<<<<< HEAD:Stql.hs
    --prints output to file if specified
    if isOutFile (last stmts) then writeFile (getOutFile (last stmts)) (sortOut (evalIt stmts triplePairs)) else return()
=======
    
    --prints output to file if specified
    if isOutFile (last stmts) then 
        writeFile (getOutFile (last stmts)) (sortOut (evalIt stmts triplePairs)) else
        return ()
>>>>>>> c39cb07071d930d55f125aaf8915b926ce9dc289:Main.hs

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

--takes a statement and returns a list of that statement and, if there, its nested statement (IN)
stmtToInnerPair :: Stmt -> [Stmt]
stmtToInnerPair s | isNothing (getInStmt s) = [s]
                  | otherwise = [s, fromJust $ getInStmt s]

--gets the filepaths of files in the statement
queryFile :: Stmt -> [FilePath]
queryFile (Stmt (Query _ f)) = getFilePaths f
queryFile (StmtOutput (Query _ f) _) = getFilePaths f
queryFile (Stmt (QueryCondition _ f _)) = getFilePaths f
queryFile (StmtOutput (QueryCondition _ f _) _) = getFilePaths f

--converts Files to list of FilePaths in Inputs folder
getFilePaths :: File -> [FilePath]
getFilePaths (File x) = [x ++ ".ttl"]
getFilePaths (Files x y) = getFilePaths x ++ getFilePaths y

--checks if statement has an output filename specified
isOutFile :: Stmt -> Bool
isOutFile (StmtOutput _ _) = True
isOutFile _ = False

--gets the output filename from the statement
getOutFile :: Stmt -> String
getOutFile (StmtOutput (Query {}) s) = s ++ ".ttl"
getOutFile (StmtOutput (QueryCondition {}) s) = s ++ ".ttl"
getOutFile _ = error "no output filename found"

--parses the raw input into an absolute triple format for evaluation
inputsToTriples :: [String] -> [Triple]
inputsToTriples = foldr
      (\ x
         -> (++)
              (stringListToTripleList
                 (onlyTriples
                    $ prefixes $ bases $ objLists $ predLists $ inputToList x)))
      []

--filters prefixes out
onlyTriples :: [String] -> [String]
onlyTriples xs = [a:as | (a:as) <- xs, a /= '@']

--parses the raw input into a list
inputToList :: String -> [String]
inputToList s = [replace x | x <- lines s, x /= ""]

--adds spaces between attributes when needed
replace :: String -> String
replace xs | Just xs <- stripPrefix "><" xs = "> <" ++ replace xs
replace (x:xs) = x : replace xs
replace [] = []

--transforms a list of attributes into a triple
stringToTriple :: [String] -> Triple
stringToTriple xs = (head xs, xs!!1, xs!!2)

--transforms each list of attributes into a triple
stringListToTripleList :: [String] -> [Triple]
stringListToTripleList = map (stringToTriple . words)

--takes statments and their triples in two lists
evalIt :: [Stmt] -> [[[Triple]]] -> [[String]]
evalIt [] _ = []
evalIt _ [] = []
evalIt (x:xs) (y:ys) = evaluator x y ++ evalIt xs ys

--takes a statement and the triples in it's file and applies select and conditions
evaluator :: Stmt -> [[Triple]] -> [[String]]
evaluator (Stmt q) ts = handleQuery q ts
evaluator (StmtOutput q s) ts = handleQuery q ts

--calls the select and rule function if needed
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

--uses the list of file contents ts pre-parsed from the main function to get the triples for the nested statement
rule (AttributeIn x y) ts = [t | t <- head ts, getAtt x t `elem` concat (evaluator (Stmt y) [ts!!1])]