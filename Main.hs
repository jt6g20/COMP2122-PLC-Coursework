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
    print triples

--evaluator (Stmt q) = evaluator q
e--valuator (StmtOutput q s) = evaluator q

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
onlyTriples xs = [a:as | (a:as) <- xs, a /= '@']

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

evaluator :: Query -> [Triple]
evaluator (QueryCondition a f c) = select a (rule c f)


select :: Attribute -> [Triple] -> [Triple]
select (Attributes x y) t = select x t ++ select y t
select Subj t = undefined--list comp
--

rule :: Condition -> [Triple] -> [Triple]
rule (ConditionAND x y) t = undefined
rule (ConditionOR x y) t = undefined
--

concatFiles :: File -> [Triple]
concatFiles (Files x y) = concatFiles x ++ concatFiles y
concatFiles (File x) = stringListToTripleList $ onlyTriples $ prefixes $ bases $ objLists $ predLists $ inputToList (readTTLFile (x ++ ".ttl"))

-- write readTTLFile function

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