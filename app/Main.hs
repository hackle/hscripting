module Main where

import System.Environment
import Samsum

main :: IO ()
main = do 
        arg1:arg2:_ <- getArgs
        let     [min, max]  = map (\x -> read x::Int) [arg1, arg2]
                result      = findBetween min max
            in putStr $ show min ++ " -> " ++ (show max) ++ "and result is " ++ (show result)
