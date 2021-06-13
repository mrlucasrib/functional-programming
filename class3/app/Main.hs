module Main where

import Lib

main :: IO ()
main = someFunc

func :: Int -> Bool
func _ = True

bools :: [Bool]
bools = [True, True]
nums :: [[Int]]
nums = [[1,2,3]]
add :: Int -> Int -> Int -> Int
add x y z = x+y+z
copy :: a -> (a, a)
copy a = (a, a)
apply :: (a -> b) -> a -> b
apply func x = func x
swap :: (a,b) -> (b,a)
swap (a, b) = (b, a)