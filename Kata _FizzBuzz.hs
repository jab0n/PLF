module FizzBuzz where

lessThan20 :: Int -> String
lessThan20 n
    | n > 0 && n <20 =  --El número es mayor a 0 y menor a 20
        let answers = words "uno dos tres cuatro cinco seis siete ocho nueve dies once doce trece catorce quince diesiceis diesisiete diesiocho diesinueve"  --Nombre de los numeros del 1 al 19
        in answers !! (n-1) --"uno dos tres cuatro cinco seis siete ocho nueve dies once doce trece catorce quince diesiceis diesisiete diesiocho diesinueve"

tens :: Int -> String
tens n
    | n > 1 && n < 10 =    --El número es mayor a 1 y menor a 10
        answers !! (n-2)
        where
            answers = words "veinti treinta cuarenta cincuenta sesenta setenta ochenta noventa"  -- Nombre de las decenas
                            --"veinte treinta cuarenta cincuenta sesenta setenta ochenta noventa"

cienes :: Int -> String
cienes n
    | n > 0 && n < 10 =    --El número es mayor a 1 y menor a 10
        answers !! (n-1)
        where
            answers = words "cien doscientos trescientos cuatrocientos quinientos seiscientos setecientos ochocientos novecientos"  -- Nombre de las decenas
                            --"veinte treinta cuarenta cincuenta sesenta setenta ochenta noventa"

isPrime :: Int -> Bool -- Descarta los primeros números 1 y 2
isPrime n
    | n <= 1        = False
    | n == 2        = True
    | otherwise     = isPrimeHelper n 2

isPrimeHelper :: Int -> Int -> Bool -- Hace un ciclo iterativo hasta que encuentra si en numero es primo o no 
isPrimeHelper n d
    | d > (n `div` 2)   = True
    | n `mod` d == 0    = False
    | otherwise         = isPrimeHelper n(d +1)

number :: Int -> String 
number n
    | n < 0 || n > 1000000                              = "Fuera del rango"     -- El número es menor a 0, por lo que no entra dentro de rango
    | isPrime(n)                                        = "FizzBuzz" -- Si el número es primo 
    | n == 0                                            = "Cero"    -- En número es igual a 0
    | n < 20                                            =  lessThan20 n     -- El número es menor a 20, por lo que solo se necesita de el metodo de 'lessThan20' para devolver su nombre
    | n < 100                                           =  tens(n `div` 10) ++ " " ++ lessThan20 (n `mod` 10)
    | n < 1000                                          =  cienes(n `div` 100) ++ " " ++ tens((n `div` 10) `mod` 10) ++ " " ++ lessThan20(n `mod` 10)
    | n < 2000                                          =  "mil " ++ cienes((n `div` 100) `mod` 10) ++ " " ++ tens((n `div` 10) `mod` 10) ++ " " ++ lessThan20(n `mod` 10)
    | n < 10000                                         =  lessThan20((n `div` 1000) `mod` 10) ++ " mil " ++ cienes((n `div` 100) `mod` 10) ++ " " ++ tens((n `div` 10) `mod` 10) ++ " " ++ lessThan20(n `mod` 10)
    | n < 100000                                        =  tens((n `div` 10000) `mod` 10) ++ " " ++ lessThan20((n `div` 1000) `mod` 10) ++ " mil " ++ cienes((n `div` 100) `mod` 10) ++ " " ++ tens((n `div` 10) `mod` 10) ++ " " ++ lessThan20(n `mod` 10)
    | n < 1000000                                       =  cienes((n `div` 100000) `mod` 10) ++ " " ++ tens((n `div` 10000) `mod` 10) ++ " " ++ lessThan20((n `div` 1000) `mod` 10) ++ " mil " ++ cienes((n `div` 100) `mod` 10) ++ " " ++ tens((n `div` 10) `mod` 10) ++ " " ++ lessThan20(n `mod` 10)
    | n == 1000000                                      = "Un millon"    
