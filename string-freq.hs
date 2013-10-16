import Data.Map as M

countChars :: String -> M.Map Char Int
countChars x = addCounts x M.empty

addCounts :: String -> M.Map Char Int -> M.Map Char Int
addCounts ""     map = map
addCounts (x:xs) map = addCounts xs (insertWith (+) x 1 map)


addCounts' :: String -> M.Map Char Int -> M.Map Char Int
addCounts' s map = Prelude.foldl (\m c -> insertWith (+) c 1 m) map s

reverseList :: [Int] -> [Int]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]
