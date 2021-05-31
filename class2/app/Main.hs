module Main where
{-# LANGUAGE BlockArguments #-}


main :: IO ()
main = do
    print(primes 1 100)
    print(max3 5 4 99)
    print(vetUnitario [5.0, 5.0])
    print(ePerfeito 8128)
    print(numerosPerfeitosIntervalo 2 28)
    print(menorQue2 [7, 8, 8, 8])
    print(tamanho [2,2])

-- Numero Perfeito
factors :: Int -> [Int]
factors n = [x | x <- [1..n-1], mod n x == 0]

ePerfeito :: Int -> Bool
ePerfeito x = sum (factors x) == x

numerosPerfeitosIntervalo :: Int -> Int -> [Int]
numerosPerfeitosIntervalo n m = [x | x <- [n..m], ePerfeito x]

-- Numeros Primos

prime :: Int -> Bool
prime n = factors n == [1]


primes :: Int -> Int -> [Int]
primes n m = [x | x <- [n..m], prime x]

-- Maior de 3

max3 :: Int -> Int -> Int -> Int
max3 x y z = max (max x y) z


-- Vetor unitario
vetUnitario :: [Float] -> [Float]
vetUnitario [x, y] = [x/v, y/v]
                where
                    v = sqrt(x**2+y**2)

-- 2 Elementos ou menos
menorQue2 :: [a] -> Bool
menorQue2 [_, _] = True
menorQue2 [_] = True
menorQue2 [] = True
menorQue2 (_:_) = False


tamanho :: [a] -> Bool
tamanho (x:xs) = length (x:xs) <= 2

    

