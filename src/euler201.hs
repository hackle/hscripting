module Euler201 where

import qualified Data.Set as Set

subsetsOf :: [a] -> [[a]]
subsetsOf [] = []
subsetsOf (x:[]) = [[x]]
subsetsOf xs@(x:rest) =
    (:) [x] $ concat $
        do  xs' <- subsetsOf rest
            return [xs', x:xs']

subsetsInLen :: [a] -> Int -> [[a]]
subsetsInLen xs len = [ ys | ys <- subsetsOf xs, len == length ys]

type SumState = (Set.Set Int, Set.Set Int)

uniqueSums :: [[Int]] -> SumState
uniqueSums xss = 
    foldl folder (Set.empty, Set.empty) xss
    where
        folder :: SumState -> [Int] -> SumState
        folder (sums, dups) xs =
            let s = sum xs in
            if Set.member s sums
            then (sums, Set.insert s dups)
            else (Set.insert s sums, dups)

findUniqueSums :: Int -> [Int] -> Set.Set Int
findUniqueSums subsetLen xs =
    let (sums, dups) = uniqueSums $ subsetsInLen xs subsetLen
    in Set.difference sums dups

findTotalUniqueSums :: Int -> [Int] -> Int
findTotalUniqueSums len xs = Set.size $ findUniqueSums len xs