module FizzBuzz where

lessThan20 :: Int -> String -- Regresa el nombre de la unidad y los numeros del 1 al 19
lessThan20 n
    | n > 0 && n <20 =  --El número es mayor a 0 y menor a 20
        let answers = words "one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen"  --Nombre de los numeros del 1 al 19
        in answers !! (n-1)

tens :: Int -> String -- Regresa el nombre de la decena
tens n
    | n > 1 && n < 10 =    --El número es mayor a 1 y menor a 10
        answers !! (n-2)
        where
            answers = words "twenty thirty forty fifty sixty seventy eighty ninety"  -- Nombre de las decenas

number :: Int -> String -- Funcuón para FizzBuzz
number n
    | n < 0 || n > 100                              = "Fuera del rango"     -- El número es menor a 0, por lo que no entra dentro de rango 
    | n == 0                                        = "zero"    --En número es igual a 0
    | n `mod` 5 == 0 && n `mod` 3 == 0 && n <= 100  = "FizzBuzz"    --El número es multiplo de 5 y de 3, ademas de ser menor o igual a 100
    | n `mod` 5 == 0 && n <= 100                    = "Fizz"    --El número es multiplo de 5, ademas de ser menor o igual a 100
    | n `mod` 3 == 0 && n <= 100                    = "Buzz"    --El número es multiplo de 3, ademas de ser menor o igual a 100
    | n > 0 && n < 20                               =  lessThan20 n     -- El número es menor a 20, por lo que solo se necesita de el metodo de 'lessThan20' para devolver su nombre
    | n > 0 && n <= 100                             =  tens(n `div` 10) ++ " " ++ lessThan20 (n `mod` 10)   --El número es menor o igual a 100 por lo que se necesita de dos metodos: 'tens' para calcular la decena y 'lessThan20' para el restante
