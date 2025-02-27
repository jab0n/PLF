import Data.Map (fromList)
-- import Statistics.Sample (mean, stdDev)

-- 1)

-- Función que aplica un descuento a un precio
aplicarDescuento :: Float -> Float -> Float
aplicarDescuento precio porcentaje = precio - (precio * porcentaje / 100)

-- Función que aplica el IVA a un precio
aplicarIVA :: Float -> Float -> Float
aplicarIVA precio porcentaje = precio + (precio * porcentaje / 100)

-- Función que recibe un diccionario (lista de pares clave-valor), y aplica una de las funciones anteriores a cada producto
calcularTotal :: [(String, Float)] -> (Float -> Float -> Float) -> Float -> Float
calcularTotal precios func porcentaje = sum [func precio porcentaje | (_, precio) <- precios]

-- Prueba:

precios = [("Manzana", 100), ("Pera", 200), ("Plátano", 150)]

-- Aplicando un 10% de descuento a los precios
descuento = calcularTotal precios aplicarDescuento 10

-- Aplicando un 16% de IVA a los precios
iva = calcularTotal precios aplicarIVA 16

-- 2) Función que aplica una función a cada elemento de una lista

aplicarFuncionALista :: (a -> b) -> [a] -> [b]
aplicarFuncionALista f lista = [f x | x <- lista]

-- 3) Función que devuelve un diccionario con las palabras y su longitud

contarPalabras :: String -> [(String, Int)]
contarPalabras frase = map (\palabra -> (palabra, length palabra)) (words frase)

-- 4) Función que convierte asignaturas y calificaciones a un diccionario con calificaciones en palabras

calificacionesEnTexto :: [(String, Float)] -> [(String, String)]
calificacionesEnTexto asignaturas = map (\(asignatura, nota) -> (asignatura, clasificarNota nota)) asignaturas
  where
    clasificarNota nota
      | nota >= 95 = "Excelente"
      | nota >= 85 = "Notable"
      | nota >= 75 = "Bueno"
      | nota >= 70 = "Suficiente"
      | otherwise  = "Desempeño insuficiente"

-- 5) Función para calcular el módulo de un vector

moduloVector :: [Float] -> Float
moduloVector vector = sqrt (sum [x^2 | x <- vector])

-- 6) Función que detecta los valores atípicos en una muestra

--valoresAtipicos :: [Float] -> [Float]
--valoresAtipicos muestra = filter (\x -> abs ((x - m) / desv) > 3) muestra
  --where
    --m = mean muestra
    --desv = stdDev muestra

-- Prueba:

main :: IO ()
main = do
  -- 1)
  print("Ejercicio 1")
  print descuento  -- Resultado con descuento
  print iva        -- Resultado con IVA
  print("")

  -- 2)
  print("Ejercicio 2")
  let lista = [1, 2, 3, 4, 5]
  print $ aplicarFuncionALista (*2) lista  -- Resultado: [2, 4, 6, 8, 10]
  print("")

  -- 3)
  print("Ejercicio 3")
  let frase = "Hola mundo en Haskell"
  print $ contarPalabras frase  -- Resultado: [("Hola", 4), ("mundo", 5), ("en", 2), ("Haskell", 7)]
  print("")

  -- 4)
  print("Ejercicio 4")
  let asignaturas = [("matematica", 96), ("historia", 80), ("quimica", 65)]
  print $ calificacionesEnTexto asignaturas  -- Resultado: [("MATEMATICA","Excelente"), ("HISTORIA","Bueno"), ("QUIMICA","Desempeño insuficiente")]
  print("")

  -- 5)
  print("Ejercicio 5")
  let vector = [3, 4]
  print $ moduloVector vector  -- Resultado: 5.0 (porque √(3² + 4²) = 5)
  print("")

  -- 6)
  print("Ejercicio 6")
  --let muestra = [10, 12, 13, 15, 100, 7, 9, 11]
  --print $ valoresAtipicos muestra  -- Resultado: [100]
