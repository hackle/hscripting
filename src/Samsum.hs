module Samsum where

import Data.Char

sumByDigits     :: [Int] -> Int
sumByDigits xs  = sum $ map charToInt $ concatMap show xs 
                    where 
                        charToInt = \c -> ord c - ord '0'

infiniteSeq             :: Int -> Int -> [Int]
infiniteSeq incr from = if from <= 0 
                        then [] 
                        else from : infiniteSeq incr (incr + from)

subsets     :: [a] -> [[a]]
subsets xs  = tail $ scanl fcons [] xs 
            where 
                fcons a b = a ++ [b]

findSeqsBySum      :: Int -> [Int] -> [[Int]]
findSeqsBySum s xs = takeWhile sumEquals $ dropWhile sumNoGreaterThan $ subsets xs
                        where
                            sumEquals ys = sumByDigits ys == s
                            sumNoGreaterThan ys = sumByDigits ys < s

findBetween         :: Int -> Int -> [(Int, Int)]
findBetween min max = 
    map firstAndLast $ concatMap findOne directions
            where
                directions = [  ([min .. max], 1), 
                                (reverse [ min .. (max - 1) ], -1) ]
                sum' = sumByDigits [ min .. max ]
                findOne (xs, incr) = concatMap (findSeqsBySum sum') $ map (infiniteSeq incr) xs
                firstAndLast xs = (head xs, last xs)
