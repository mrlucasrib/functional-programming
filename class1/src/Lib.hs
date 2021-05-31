{-# LANGUAGE BlockArguments #-}

module Lib where

someFunc :: IO ()
someFunc = do
  print (fib 3)
  print (fibLinear 4)
  print (sumOfSquares 5 4 6)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n -1) + fib (n - 2)

fibLinear :: Int -> Int
fibLinear 0 = 0
fibLinear 1 = 1
fibLinear n
  | even n = f1 * (f1 + 2 * f2)
  | n `mod` 4 == 1 = (2 * f1 + f2) * (2 * f1 - f2) + 2
  | otherwise = (2 * f1 + f2) * (2 * f1 - f2) - 2
  where
    k = n `div` 2
    f1 = fibLinear k
    f2 = fibLinear (k -1)


sumOfSquares :: Int -> Int -> Int -> Int
sumOfSquares x y z  | x >= y && y >= z =  x^2 + y^2
                    | x >= y && x >= z && z >= y =  x^2 + z^2
                    | y >= x && x >= z =  y^2 + x^2
                    | y >= x && y >= z && x < z  =  y^2 + z^2
                    | z >= x && x >= y  =  z^2 + x^2
                    | z >= x && z >= y && y >= x  =  z^2 + y^2
                    | otherwise = -1