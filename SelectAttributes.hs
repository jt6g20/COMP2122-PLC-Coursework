module SelectAttributes where
import Grammar
import Utilities

--SELECT clause: filters the specified attribute(s) from each triple
select :: Attribute -> [Triple] -> [[String]]
select a t = [x | x <- xs, ":(" `notElem` x]
    where xs = map (selectTriple a) t

--filters each specified attribute
selectTriple :: Attribute -> Triple -> [String]
selectTriple (Attributes a as) t = getAtt a t : selectTriple as t
selectTriple a t = [getAtt a t]