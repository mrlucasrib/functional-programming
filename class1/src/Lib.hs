{-# LANGUAGE BlockArguments #-}

module Lib where

someFunc :: IO ()
someFunc = do
  print (fib 5)
  print (fibLinear 5)
  print (sumOfSquares 5 4 6)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n -1) + fib (n - 2)

fibLinear :: Float -> Float
fibLinear 0 = 0
fibLinear 1 = 1
fibLinear n = ((1 + sqrt 5)**n - (1- sqrt 5)**n)/(2**n * sqrt 5)


sumOfSquares :: Int -> Int -> Int -> Int
sumOfSquares x y z  | x >= y && y >= z =  x^2 + y^2
                    | x >= y && x >= z && z >= y =  x^2 + z^2
                    | y >= x && x >= z =  y^2 + x^2
                    | y >= x && y >= z && x < z  =  y^2 + z^2
                    | z >= x && x >= y  =  z^2 + x^2
                    | z >= x && z >= y && y >= x  =  z^2 + y^2
                    | otherwise = -1