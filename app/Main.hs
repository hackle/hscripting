module Main where

import System.Environment
import Euler201
import Text.Printf
import Data.Time.Clock

main :: IO ()
main = do 
        startTime <- getCurrentTime
        arg1:arg2:arg3:_ <- getArgs
        let     [min, max, len]  = map (\x -> read x::Int) [arg1, arg2, arg3]
                cnt = findUniqueSums len [min..max] in
                do      finishTime <- getCurrentTime
                        let     msg = printf "Result: %s Time elapsed: %s" (show cnt) (show $ diffUTCTime startTime finishTime) in
                                putStr msg

                
                
