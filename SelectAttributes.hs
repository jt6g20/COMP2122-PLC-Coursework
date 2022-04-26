module SelectAttributes where
import Grammar
import Utilities
import Text.Read

-- Stmt (QueryCondition (Attributes Subj (Attributes Pred (AttributeObj Obj))) (File "foo") (ConditionOR (AttributeEq Pred (AttributeString "http://www.cw.org/problem3/#predicate1")) (ConditionOR (AttributeEq Pred (AttributeString 
-- "http://www.cw.org/problem3/#predicate2")) (AttributeEq Pred (AttributeString "http://www.cw.org/problem3/#predicate3")))))

subjLookup :: Triple -> String
subjLookup (x, _, _) = x
predLookup :: Triple -> String
predLookup (_, x, _) = x
objLookup :: Triple -> String
objLookup (_, _, x) = x

select :: Attribute -> [Triple] -> [[String]]
select a t = [x | x <- xs, ":(" `notElem` x]
    where xs = map (selectTriple a) t

-- listToTriple :: [String] -> Triple
-- listToTriple (x:y:z:_) = (x,y,z)
-- listToTriple [x, y] = (x,y,"_")
-- listToTriple [x] = (x,"_","_")
-- listToTriple _ = ("_", "_", "_")

selectTriple :: Attribute -> Triple -> [String]
selectTriple (Attributes a as) t = getAttS a t : selectTriple as t
selectTriple a t = [getAttS a t]

getAttS :: Attribute -> Triple -> String
getAttS (AttributeString s) _ = "<" ++ s ++ ">"
getAttS x t = getAtt x t