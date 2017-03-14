module LongestCombo where

import Data.Function
import Data.List

maxTolerance = 2

longestList :: [[a]] -> [a]
longestList xss = maximumBy (compare `on` length) xss

findComboWithTolerance :: Int -> [Int] -> [Int]
findComboWithTolerance tolerance _ | tolerance > maxTolerance = []
findComboWithTolerance _ [] = []
findComboWithTolerance _ (x:[]) = [x]
findComboWithTolerance tolerance (x:x1:xs) =  if inOrder
                                then longestList [ thisContinued, moreTolerant ]
                                else findComboWithTolerance (tolerance + 1) (x:xs)
                                    where
                                        inOrder = x < x1
                                        thisContinued = x : (findComboWithTolerance 0 (x1:xs))
                                        moreTolerant = findComboWithTolerance (tolerance + 1) (x:xs)

longestCombo :: [Int] -> [Int]
longestCombo xs = longestList [ findComboWithTolerance 0 ys | ys <- scanr (:) [] xs ]

-- longestCombo [ 1, 2, 3, 2, 6, 4, 7, 6, 6, 7, 10, 11 ] = [1,2,3,4,6,7,10,11]
-- longestCombo [1, 2, 3, 5, 4, 5] = [1,2,3,4,5]
-- longestCombo [1, 2, 3, 5, 5, 5, 4, 5] = [1,2,3,5]