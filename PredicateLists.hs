module PredicateLists where
import Utilities

predListEvaluator :: String -> [String] -> [String]
predListEvaluator "" (x:xs) = x : predListEvaluator (head $ words x) xs
predListEvaluator subj (x:xs) = (subj ++ x) : predListEvaluator subj xs
predListEvaluator _ _ = []

predicateList :: String -> [String]
predicateList s = predListEvaluator "" $ splitOn ';' s

predLists :: [String] -> [String]
predLists (x:xs) | ';' `elem` x = predicateList x ++ predLists xs
                       | otherwise = x : predLists xs
predLists _ = []