module Part2 where

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = foldr (\n acc -> if null acc || snd (head acc) /= n then (1, n):acc else ((fst $ head acc) + 1, n):(tail acc) ) [] xs


-- Problem 11
data Counter a = Multi Int a | Single a deriving(Show)

modifiedEncode :: Eq a => [a] -> [Counter a]
modifiedEncode = map helper . encode
    where
        helper (1, x) = Single x
        helper (n, x) = Multi n x

encodeHelper :: Eq a => (Int, a) -> Counter a
encodeHelper (1, x) = Single x
encodeHelper (n, x) = Multi n x

-- Problem 12
modifiedDecode :: [Counter a] -> [a]
modifiedDecode = concatMap decode
    where
        decode :: Counter a -> [a]
        decode (Multi n x) = replicate n x
        decode (Single x) = [x]

-- Problem 13
-- Revisit this.  P11 built the encoding from sublists.  this should be
-- done without sublists.
encodeDirect :: Eq a => [a] -> [Counter a]
encodeDirect = map encodeHelper . encode

-- Problem 14
dupli :: [a] -> [a]
dupli = foldr (\x acc -> x:x:acc) []

-- Problem 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli [x] n = replicate n x
repli (x:xs) n = replicate n x ++ repli xs n

-- simpler version
repli' :: [a] -> Int -> [a]
repli' xs n = concatMap (replicate n) xs

-- Problem 16
dropIndex :: [a] -> Int -> [a]
dropIndex [] _ = []
dropIndex xs n = (take (n - 1) xs) ++ dropIndex (drop n xs) n

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split xs n = if length xs >= n
             then
                 (take n xs,  drop n xs)
             else
                 (xs, [])
-- take and drop are lazy, so there is no need to check the length
-- split xs n = (take n xs,  drop n xs)
-- below we wont use these predifined predicates
split' :: [a] -> Int -> ([a], [a])
split' (x:xs) n | n > 0 = let (b, e) = split' xs (n-1)
              in (x:b, e)
split' xs _ = ([], xs)

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
-- this is first line is not necessary
-- slice _ b e | b >= e = []
slice xs b e = drop (b-1) $ take e xs

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate xs n
        | n >= 0 = let (begin, end) = splitAt n xs in end ++ begin
        | n < 0  = let (begin, end) = splitAt ((length xs) + n) xs in end ++ begin

