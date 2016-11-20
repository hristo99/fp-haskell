{-# OPTIONS_GHC -Wall #-}

module Lists where

import qualified Data.List ()

import Prelude hiding (
        head, tail, null, length, reverse, repeat, replicate,
        concat, sum, maximum, take, drop, elem, (!!)
    )


head :: [Int] -> Int
head []    = error "empty list"
head (x:_) = x


tail :: [Int] -> [Int]
tail []     = error "empty list"
tail (_:xs) = xs


append :: [Int] -> [Int] -> [Int]
append []     ys = ys
append (x:xs) ys = x : (append xs ys)


elementAt :: Int -> [Int] -> Int
elementAt 0 (x:_) = x
elementAt _ []     = error "index greater than length"
elementAt n (_:xs) = elementAt (n - 1) xs


null :: [Int] -> Bool
null [] = True
null _  = False

length :: [Int] -> Int
length [] = 0
length (_:xs) = 1 + length xs


take :: Int -> [Int] -> [Int]
take _ [] = []
take 1 (x:_) = [x]
take n _ | n < 1 = []
take n (x:xs) = x : take (n-1) xs

take' :: Int -> [Int] -> [Int]
take' 0 _  = []
take' _ [] = []
take' n (x:xs) | n < 0     = []
               | otherwise = x : take' (n-1) xs


drop :: Int -> [Int] -> [Int]
drop _ [] = []
drop n (x:xs) | n <= 0 = (x:xs)
              | otherwise = drop (n-1) xs


elem :: Int -> [Int] -> Bool
elem _ [] = False
elem e (x:xs) | e == x = True
              | otherwise = elem e xs


reverseHelper :: [Int] -> [Int] -> [Int]
reverseHelper acc []     = acc
reverseHelper acc (x:xs) = reverseHelper (x:acc) xs

reverse :: [Int] -> [Int]
reverse xs = reverseHelper [] xs

reverseStringHelper::String->String->String
reverseStringHelper acc [] = acc
reverseStringHelper acc (x:xs) = (reverseStringHelper (x:acc) xs)


reverseString :: String -> String
reverseString str = "This is the reversed string: " ++ (reverseStringHelper [] str)

concat :: [[Int]] -> [Int]
concat [] = []
concat (x:xs) = x ++ concat xs


replicate :: Int -> Int -> [Int]
replicate n x | n <= 0 = []
              | otherwise = x : replicate (n-1) x


interleave :: [Int] -> [Int] -> [Int]
interleave [] _ = []
interleave (x:_) [] = [x]
interleave (x:xs) (y:ys) = x : y : interleave xs ys


sum :: [Int] -> Int
sum (x:xs) = x + sum xs
sum [] = 0


maximum :: [Int] -> Int
maximum [] = error "empty list"
maximum (x:xs) | xs == [] || x >= maximum xs = x
               | otherwise = maximum xs

nub :: [Int] -> [Int]
nub [] = []
nub (x:xs) = x : nub (nub' x xs)

nub' :: Int -> [Int] -> [Int]
nub' _ [] = []
nub' n (x:xs) | n == x = nub' n xs
              | otherwise = x : nub' n xs

delete :: Int -> [Int] -> [Int]
delete _ [] = []
delete n (x:xs) | n == x = xs
                | otherwise = x : delete n xs


difference :: [Int] -> [Int] -> [Int]
difference [] _ = []
difference l [] = l
difference (x:xs) l | elem x l = difference xs (delete x l)
                    | otherwise = x : difference xs l

union :: [Int] -> [Int] -> [Int]
union l m = l ++ nub (difference m l)


intersect :: [Int] -> [Int] -> [Int]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) l | elem x l = x : intersect xs l
                   | otherwise = intersect xs l
