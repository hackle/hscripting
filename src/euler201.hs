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

uniqueSums :: [[Int]] -> Set.Set Int
uniqueSums xss = 
    foldl folder Set.empty xss
    where
        folder :: Set.Set Int -> [Int] -> Set.Set Int
        folder set xs =
            let s = sum xs in
            if Set.member s set
            then Set.delete s set
            else Set.insert s set

findUniqueSums :: Int -> [Int] -> Set.Set Int
findUniqueSums subsetLen xs =
    uniqueSums $ subsetsInLen xs subsetLen