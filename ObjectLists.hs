module ObjectLists where
import Utilities

--transforms object lists to include their common subjects and predicates into separate lines
objListEvaluator :: String -> String -> [String] -> [String]
objListEvaluator "" "" (x:xs) = x : objListEvaluator (head $ words x) (head $ tail $ words x) xs
objListEvaluator subj pred (x:xs) = (subj ++ " " ++ pred ++ x) : objListEvaluator subj pred xs
objListEvaluator _ _ _ = []

objList :: String -> [String]
objList s = objListEvaluator "" "" $ splitOn ',' s

objLists :: [String] -> [String]
objLists (x:xs) | ',' `elem` x = objList x ++ objLists xs
                | otherwise = x : objLists xs
objLists _ = []