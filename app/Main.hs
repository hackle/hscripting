module Main where

import System.Environment
import Samsum
import Text.Printf

main :: IO ()
main = do 
        arg1:arg2:_ <- getArgs
        let     [min, max]  = map (\x -> read x::Int) [arg1, arg2]
                result      = findBetween min max
            in putStr $ printf "%d -> %d and result is %s" min max (show result)
