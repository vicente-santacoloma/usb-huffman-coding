-- | Modulo Huffman implementacion de las funciones necesarias
-- | para modelar la estructura de datos Arbol de Huffman
-- | que en conjunto con el archivo CompresorDeArchivos.hs 
-- | completan la funcionalidad siguiente:
-- | Dado un archivo codificarlo y dado un archivo codificado decodificarlo
-- | Andreth Salazar, Vicente Santacoloma
module Huffman where 

import LeftistHeap
-- | Un arbol de huffman puede estar representado por una hoja, la cual posee
-- | la frecuencia de un caracter en un string + el caracter mismo o una rama
-- | que posee la frecuencia acumulada de sus dos arboles siguientes o hijos.
data Huffman a = Hoja Integer a | Rama Integer (Huffman a) (Huffman a) deriving Show

-- | ObtenerFrecuencia como su nombre lo indica obtiene la frecuencia de un arbol
obtenerFrecuencia:: Huffman a -> Integer
obtenerFrecuencia (Rama x _ _) = x
obtenerFrecuencia (Hoja x _) = x

instance Eq (Huffman a) where
  a == b = obtenerFrecuencia a == obtenerFrecuencia b
  
instance Ord (Huffman a) where
  a <  b  = obtenerFrecuencia a < obtenerFrecuencia b
  a <= b  = (a < b) || (a == b)

-- | crearRama dado dos arboles de huffman crea una rama que posee los dos arboles
crearRama :: Huffman a -> Huffman a -> Huffman a 
crearRama huf1 huf2 = (Rama (obtenerFrecuencia(huf1)+obtenerFrecuencia(huf2)) huf1 huf2)

-- | crearHuffman dado una lista de pares (caracter,frecuencia) contruye un Huffman utilizando la implementacion
-- | LeftistHeap para la cola de prioridades y los metodos de obtener minimos y borrar minimo
crearHuffman :: [(a, Integer)] -> Huffman a
crearHuffman list = crear (inicializar list)
  where
    inicializar l = foldl (\ a (x,i) -> insert (Hoja i x) a) E l
    crear a
      | isEmpty (deleteMin a) = findMin a
      | otherwise = crear (insert (crearRama (findMin a) (findMin (deleteMin a))) (deleteMin (deleteMin a)))

-- | obtenerElementos dado un arbol de Huffman devuelve una lista de elementos con sus caracteres
obtenerElementos :: Huffman t -> [t]
obtenerElementos (Hoja _ a) = [a]
obtenerElementos (Rama _ a b) = (obtenerElementos a) ++ (obtenerElementos b)

-- | pertenece funcion que determina si un caracter pertenece a un arbol de huffman 
pertenece :: Eq a => a -> Huffman a -> Bool
pertenece x (Hoja _ a)
  | x == a = True
  | otherwise = False
pertenece x (Rama _ a b) = (pertenece x a) || (pertenece x b)

-- | obtenerCodificacion Dado un elemento  y un arbol de huffman devuelve el camino
-- | booleano que representa recorrer el arbol hasta llegar al caracter mas un True
obtenerCodificacion :: Eq a => a -> Huffman a -> [Bool]
obtenerCodificacion _ (Hoja _ _) = [True]
obtenerCodificacion x (Rama _ a b)
  | pertenece x a = [False] ++ (obtenerCodificacion x a)
  | pertenece x b = [True]  ++ (obtenerCodificacion x b)
  
-- | generarAsociaciones dado un arbol de huffman devuelve una lista 
-- | de pares (caracter,caminoBooleano) que representa la codificacion de
-- | cada caracter sobre el arbol
generarAsociaciones :: Eq a => Huffman a -> [(a,[Bool])]
generarAsociaciones a = [(x,obtenerCodificacion x a) | x <- (obtenerElementos a)]

-- | calcularSuma dado un caracter un numero y otro caracter si son iguales
-- | suma uno en otros casos no suma 
calcularSuma :: (Num a1, Eq a) => a -> a1 -> a -> a1
calcularSuma a x b
  | a == b = x+1
  |otherwise = x

-- | obtenerRepeticion obtiene la frecuencia de un caracter dentro de una lista de caracteres
obtenerRepeticion :: (Num a, Eq b) => b -> [b] -> a
obtenerRepeticion a list = foldl (calcularSuma a) 0 list

-- | obtenerAgrupacion contruye una lista con pares caracter numero con la repeticion que posee
-- | un numero dentro de un string
obtenerAgrupacion :: (Num t, Eq a) => [a] -> [(a, t)]
obtenerAgrupacion [] = []
obtenerAgrupacion (x:xs) = [(x,(obtenerRepeticion x xs) +1)] ++ obtenerAgrupacion([y | y <- xs, y /= x])

-- | obtenerCodigo dado un elemento compara en una lista de pares
-- | al primero y devuelve si son iguales el segundo
obtenerCodigo :: Eq a => a -> [(a, t)] -> t
obtenerCodigo x ((a,b):xs)
  | x == a = b
  | otherwise = obtenerCodigo x xs
  
-- | codificar dado una lista de caracteres devuelve un arbol de huffman y un camino que contiene
-- | El arbol de huffman rerpresenta el string codificado
-- | el camino representa el recorrido del string dentro del arbol
codificar :: Eq a => [a] -> (Huffman a, [Bool])
codificar list = inicializarCodificacion (obtenerAgrupacion list) list
  where
    inicializarCodificacion l list = construirCodificacion (crearHuffman l) list
    construirCodificacion a list = (a, concatenarCodificacion (generarAsociaciones a) list)
    concatenarCodificacion codificacionHash list = concat [obtenerCodigo a codificacionHash | a <- list]

-- | reconstruirHuffman dado una lista de pares (caracter,booleano) devuelve un Huffman que representa
-- | esta lista de pares.
reconstruirHuffman :: [(a,[Bool])] -> Huffman a
reconstruirHuffman [(x,xs)] = (Hoja 0 x)
reconstruirHuffman list = (Rama 0 (reconstruirHuffman (factorizar list False)) (reconstruirHuffman (factorizar list True)))
  where 
    factorizar list bool = [(x, tail xs) | (x,xs) <- list , (head xs) == bool]

-- | decodificar dado un arbol de huffman y una lista de recorridos sobre el devuelve el string que representa
-- | la decodificacion del mismo.
decodificar:: Huffman a -> [Bool] -> [a]
decodificar tree code = recorrer (code,tree,tree)

-- | recorre un arbol de huffman en base a los caminos dados
recorrer ::  ([Bool] , Huffman a , Huffman a) -> [a]
recorrer ([],_,_) = []
recorrer ((True:xs),tree,(Hoja _ x)) = [x]++recorrer(xs,tree,tree)
recorrer ((False:xs),tree,(Rama _ x y) ) = recorrer(xs,tree,x)
recorrer ((True:xs),tree,(Rama _ x y)) = recorrer(xs,tree,y)