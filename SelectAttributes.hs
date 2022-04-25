module SelectAttributes where
import Grammar
import Utilities

-- Stmt (QueryCondition (Attributes Subj (Attributes Pred (AttributeObj Obj))) (File "foo") (ConditionOR (AttributeEq Pred (AttributeString "http://www.cw.org/problem3/#predicate1")) (ConditionOR (AttributeEq Pred (AttributeString 
-- "http://www.cw.org/problem3/#predicate2")) (AttributeEq Pred (AttributeString "http://www.cw.org/problem3/#predicate3")))))

subjLookup :: Triple -> String
subjLookup (x, _, _) = x
predLookup :: Triple -> String
predLookup (_, x, _) = x
objLookup :: Triple -> String
objLookup (_, _, x) = x

select :: Attribute -> [Triple] -> [Triple]
select a = map (listToTriple . selectTriple a)

listToTriple :: [String] -> Triple
listToTriple (x:y:z:_) = (x,y,z)
listToTriple [x, y] = (x,y,"_")
listToTriple [x] = (x,"_","_")

selectTriple :: Attribute -> Triple -> [String]
selectTriple (Attributes a as) t = tripleIsolate a t : selectTriple as t
selectTriple a t = [tripleIsolate a t]

tripleIsolate :: Attribute -> Triple -> String
tripleIsolate Subj t = subjLookup t
tripleIsolate Pred t = predLookup t
tripleIsolate (AttributeObj Obj) t = objLookup t
tripleIsolate (AttributeObj (ObjAdd i)) t = show $ (objNumToInt $ objLookup t) + i
tripleIsolate (AttributeString s) _ = s
tripleIsolate (AttributeBoolean b) _ = show b

objNumToInt :: String -> Int
objNumToInt (x:xs) | x == '+' = read xs
objNumToInt s = read s