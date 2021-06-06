module Main where

import Lib

main :: IO ()
main = do
    print(minList [3,5,7,67])
    print(andList [True, True, False])
    print(orList [False, False, False])
    print(indexOf [3,5,7, 4, 5, 2, 6, 2] 68)
    print(takeList [3, 5, 6 ,7 ,2] 3)
    print(dropList [3, 5, 6 ,7 ,2] 3)
    print(removeAll [3, 5, 6 ,7, 3 ,2] 3)
    print(removeAllc [3, 5, 6 ,7, 3 ,2] 3)
    print(countPos [3, 5, 6 ,7, 3 ,2])


-- Menor elemento de uma lista
minList :: [Int] -> Int
minList (x:xs) = minList' (x:xs) x
    where
        minList' [] x = x
        minList' (z:zs) x = minList' zs (min x z)


-- Conjunção de uma lista de boleanos
andList :: [Bool] -> Bool
andList [] = True
andList (x:xs)
    | x = andList xs
    | otherwise = False


-- Dijunção de uma lista de boleanos
orList :: [Bool] -> Bool
orList [] = False
orList (x:xs)
    | x = True
    | otherwise = orList xs


-- Posisão de um item na lista
indexOf :: [Int] -> Int -> Int
indexOf (x:xs) y = indexOf' (x:xs) y 0 -- Acumulador
    where
        indexOf' [] _ _ = -1 -- Transformar em um if xs == []
        indexOf' (x:xs) y acc
            | x == y = acc
            | otherwise = indexOf' xs y (acc+1)


-- Primeiros n elementos de uma lista
takeList :: [Int] -> Int -> [Int]
takeList xs y = takeList' xs y []
    where
        takeList' (x:xs) y k
            | length k == y = k
            | otherwise = takeList' xs y (k ++ [x])


-- Remove os n primeiros elementos de uma lista
dropList :: [Int] -> Int -> [Int]
dropList xs y = dropList' xs y 1
    where
        dropList' (x:xs) y k
            | k == y = xs
            | otherwise = dropList' xs y (k+1)


-- Remove um item de uma lista
removeAll :: [Int] -> Int -> [Int]
removeAll xs y = removeAll' xs y []
    where
        removeAll' [] _ k = k
        removeAll' (x:xs) y k
            | y == x = removeAll' xs y k
            | otherwise = removeAll' xs y (k ++ [x])
            
-- Remove um item de uma lista com list comprehension
removeAllc :: [Int] -> Int -> [Int]
removeAllc xs y = [k | k <- xs, y /= k]


-- Retorna a quantidade de numeros inteiros positivos
countPos :: [Int] -> Int
countPos xs = length [x | x <- xs, x >= 0]