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

{-SELECT FILES 1, 2
SELECT FILE 3
SELECT FILE 4, 5, 6
[[1FP, 2FP], [3FP], [4FP, 5FP, 6FP]] = inputFiles
[[1Str, 2Str], [3Str], [4Str, 5Str, 6Str]] = contents
[12T, 3T, 456T] = triples
[S1, S2, S3]-}
main :: IO ()
-- main = catch lexer handler
main = do
    (fileName : _) <- getArgs
    stmtString <- readFile fileName

    let stmts = map (parseSQL . alexScanTokens) (lines stmtString)
    let inputFiles = map queryFile stmts
    contents <- mapM (mapM readFile) inputFiles

    let triples = map (inputsToTriples) contents

    print (sortOut $ evalIt stmts triples)
    --if isOutFile (last stmts) then writeFile (getOutFile (last stmts)) (sortOut (evalIt stmts triples)) else print (sortOut (evalIt stmts triples))

-- Stmt (QueryCondition (Attributes Subj (Attributes Pred (AttributeObj Obj))) (File "foo") (ConditionAND (AttributeEq Subj (AttributeString "<http://www.cw.org/#problem2>")) (AttributeEq (AttributeObj Obj) (AttributeBoolean True))))

-- Stmt (QueryCondition (Attributes Subj (Attributes Pred (AttributeObj Obj))) (File "foo") (ConditionOR (AttributeEq Pred (AttributeString "http://www.cw.org/problem3/#predicate1")) (ConditionOR (AttributeEq Pred (AttributeString 
-- "http://www.cw.org/problem3/#predicate2")) (AttributeEq Pred (AttributeString "http://www.cw.org/problem3/#predicate3")))))

queryFile :: Stmt -> [FilePath]
queryFile (Stmt (Query _ f)) = getFilePaths f
queryFile (StmtOutput (Query _ f) _) = getFilePaths f
queryFile (Stmt (QueryCondition _ f _)) = getFilePaths f
queryFile (StmtOutput (QueryCondition _ f _) _) = getFilePaths f

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

--sortOut :: [[String]] -> [String]
sortOut xss = concatMap (\x -> join x ++ " .\n") (sortAtts (rmvDupl xss))

sortAtts :: [[String]] -> [[String]]
sortAtts (xs:xss) | length xs == 3 = sortAtt $ sortAtt $ sortAtt xss
                  | length xs == 2 = sortAtt $ sortAtt xs
                  | length xs == 1 = sortAtt xs
                  | otherwise = error "invalid triple"

sortAtt :: [[String]] -> [[String]]
sortAtt = undefined

rmvDupl :: Eq a => [[a]] -> [[a]]
rmvDupl [] = []
rmvDupl (xs:xss) | (allDupl 0 xs xss) = rmvDupl xss
                 | otherwise = xs : rmvDupl xss

allDupl :: Eq a => Int -> [a] -> [[a]] -> Bool
allDupl n [] xss = True
allDupl n (x:xs) xss = x `elem` (map (!!n) xss) && (allDupl (n+1) xs xss)

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

evalIt :: [Stmt] -> [[Triple]] -> [[String]]
evalIt [] _ = []
evalIt _ [] = []
evalIt (x:xs) (y:ys) = evaluator x y ++ evalIt xs ys

evaluator :: Stmt -> [Triple] -> [[String]]
evaluator (Stmt q) ts = handleQuery q ts
evaluator (StmtOutput q s) ts = handleQuery q ts

join :: [String] -> String
join [x,y,z] | "<" `isPrefixOf` z = x ++ y ++ z
             | otherwise = x ++ y ++ " " ++ z
join [x,y] | "<" `isPrefixOf` y = x ++ y
           | otherwise = x ++ " " ++ y
join _ = error "out of scope"

handleQuery :: Query -> [Triple] -> [[String]]
handleQuery (QueryCondition a f c) ts = select a (rule c ts)
handleQuery (Query a f) ts = select a ts

--WHERE clause: uses list comprehension to filter triples with given conditions 
rule :: Condition -> [Triple] -> [Triple]
rule (ConditionAND x y) ts = rule x ts `intersect` rule y ts
rule (ConditionOR x y) ts = rule x ts `union` rule y ts

--checks whether object attribute is integer before comparison
rule (Greater x@(AttributeObj Obj) n) ts = [t | t <- ts, (readMaybe (getAtt x t) :: Maybe Int) > Just n]
--only Obj can be Int
rule (Greater x n) ts = error ("invalid attribute " ++ show x ++ " to compare '>' with " ++ show n)

--slightly different implementation to Greater as Maybe type declares Nothing < Just n = True
rule (Less x@(AttributeObj Obj) n) ts = [t | t <- ts, less (readMaybe (getAtt x t) :: Maybe Int) n]
    where less x n | isJust x = x < Just n
                   | otherwise = False -- Monad??
rule (Less x n) ts = error ("invalid attribute " ++ show x ++ " to compare '<' with " ++ show n)

rule (NumEq x@(AttributeObj Obj) n) ts = [t | t <- ts, (readMaybe (getAtt x t) :: Maybe Int) == Just n]
rule (NumEq x n) ts = error ("invalid attribute " ++ show x ++" to compare '=' with " ++ show n)

rule (AttributeEq x y) ts = [t | t <- ts, getAtt x t == getAtt y t]
--    where rmvBrackets (AttributeString x) = init (tail x)
--          rmvBrackets x = x
rule (AttributeIn x y) ts = [t | t <- ts, getAtt x t `elem` evaluator y]
    where evaluator x = ["placeholder", "function"]


{-
data Stmt = Stmt Query
          | StmtOutput Query String
            deriving Show 
data Query = QueryCondition Attribute File Condition
            | Query Attribute File
            deriving Show 
data File = Files File File
            | File String
            deriving Show 
data Condition = ConditionAND Condition Condition
                | ConditionOR Condition Condition
                | Greater Attribute Int
                | Less Attribute Int
                | NumEq Attribute Int
                | AttributeEq Attribute Attribute
                | AttributeIn Attribute Query
                deriving Show
data Attribute = Attributes Attribute Attribute
                | Subj
                | Pred
                | AttributeObj Obj
                | AttributeString String
                | AttributeBoolean Bool
                deriving Show
data Obj = Obj
         | ObjAdd Int
         deriving Show
}
-}