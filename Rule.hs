module Rule where
import Grammar
import Utilities
import Text.Read
import Data.Maybe
import Data.Char
import Data.List

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
rule (AttributeIn x y) ts = [t | t <- ts, getAtt x t `elem` evaluator y]
    where evaluator x = ["placeholder", "function"]

--gets specified attribute from triple, and just returns constant as is
getAtt :: Attribute -> Triple -> String
getAtt Subj (x, _, _) = x
getAtt Pred (_, x, _) = x
getAtt (AttributeObj Obj) (_, _, x) = x
getAtt (AttributeString s) x = s
getAtt (AttributeBoolean b) x = map toLower (show b) --ttl uses lowercase for booleans
getAtt _ _ = error "invalid attribute for WHERE" --Attributes and AtttributeObj ObjAdd not allowed