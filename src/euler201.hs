module Euler201 where

import qualified Data.Set as Set

subsetsOf :: [a] -> Int -> [[a]] -> [[a]]
subsetsOf [] len acc = filter (\xs -> length xs == len) acc
subsetsOf xs@(x:rest) len acc =
    subsetsOf rest len acc'
    where
        acc' = [x] : acc ++ [ x:ys | ys <- acc, length ys < len ]

type SumState = (Set.Set Int, Set.Set Int)

uniqueSums :: [[Int]] -> SumState
uniqueSums xss = 
    foldl folder (Set.empty, Set.empty) xss
    where
        folder :: SumState -> [Int] -> SumState
        folder state@(sums, dups) xs =
            let s = sum xs in
            if Set.member s dups
            then state
            else    
                if Set.member s sums
                then (sums, Set.insert s dups)
                else (Set.insert s sums, dups)

findUniqueSums :: Int -> [Int] -> Int
findUniqueSums subsetLen xs =
    let (sums, dups) = uniqueSums $ subsetsOf xs subsetLen []
    in Set.size sums - Set.size dups