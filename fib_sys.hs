-- Implementation of fibonacci number system
-- 
-- Every number can be expressed as a sum of some
-- fibonacci numbers. 
-- Fibonacci system encodes number as a list of indexes
-- in fibonacci sequence, which is:
-- 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, ...
--
-- Example: 
-- number 50 can be expressed as the sum of 3, 13 and 34
-- or indexes 4, 7 and 9 respectively
-- 50_10 = [4,7,9]_fib

module Main where

-- Infinite lazy list of fibonacci numbers
fib :: [Integer]
fib = 0 : scanl (+) 1 fib

-- Returns list of indexes, where sum of corresponding
-- fibonacci numbers is equal to encoded number
encode :: Integer -> [Int]
encode n = reverse $ find n fibs
    where 
        fibs = takeWhile (n >=) fib
        find num xs
            | lastElem == 0 = []
            | otherwise     = (length xs - 1) : (find diff $ takeWhile (diff >=) xs)
            where
                diff     = num - lastElem
                lastElem = last xs

-- Takes a list of indexes and returns a sum of corresponding
-- fibonacci numbers
decode :: [Int] -> Integer
decode c = sum $ map (xs !!) c
    where xs = take (last c + 1) fib

main :: IO ()
main = do
    print $ encode 50
    print $ decode $ encode 50
