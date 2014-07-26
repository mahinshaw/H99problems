module Part1 where

import Data.List (group)

-- Problem 1
myLast :: [a] -> a
-- myLast = head . reverse
-- insert so that i dont have to hear about the errors
myLast = last

myLast' :: [a] -> a
myLast' [x] = x
myLast' (x:xs) = myLast' xs

-- Problem 2
butLast :: [a] -> a
butLast (x:y:[]) = x
butLast (x:y:xs) = butLast (y:xs)

butLast' :: [a] -> a
butLast' = head . tail . reverse

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt xs n = xs !! (n-1)

-- Problem 4
myLength1:: [a] -> Int
myLength1 = foldl (\n _ -> n + 1) 0

myLength2:: [a] -> Int
myLength2 = foldr (\_ n -> n + 1) 0

-- Problem 5
myReverse :: [a] -> [a]
myReverse = foldl (\n acc -> acc:n) []

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []  = True
isPalindrome [_] = True
isPalindrome xs  = head xs == last xs && (isPalindrome $ tail . init $ xs)

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x)      = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List [])     = []

-- Problem 8
compress :: (Eq a) =>  [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = if x `elem` xs
                  then compress xs
                  else x:(compress xs)

-- This will only work for consecutive duplicates
compress' :: (Eq a) => [a] -> [a]
compress' [] = []
compress' (x:xs) = x : (compress' $ dropWhile (== x) xs)

compress'' :: Eq a => [a] -> [a]
compress'' xs = map head $ group xs

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (begin, end) = span (==x) xs
                  in (x:begin):(pack end)

pack' :: Eq a => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = (x:takeWhile (== x) xs) : (pack $ dropWhile (==x) xs)

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = foldr (\n acc -> if null acc || snd (head acc) /= n then (1, n):acc else ((fst $ head acc) + 1, n):(tail acc) ) [] xs

encode' :: Eq a => [a] -> [(Int, a)]
encode' xs = map (\x -> (length x, head x)) $ group xs
--encode' xs = [(length x, head x) | x <- group xs]


