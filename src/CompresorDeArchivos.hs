{-
 -    Este m�dulo comprende la funcionalidad necesaria para comprimir/descomprimir
 - archivos utilizando un algoritmo de codificaci�n de Huffman.
 - 
 - Autor(es): Ricardo Monascal, Gabriela Montoya, Eleazar Leal
 - Version: 1.0
 -}
module CompresorDeArchivos (
	leerDeArchivo,
	imprimirArchivo,
	transformar
)
where
import Huffman
import Data.Word
import Data.List
import System
import qualified Data.ByteString as BS

{-
 - Un byte es una palabra de 8 bits.
 -}
type Byte = Data.Word.Word8

{-
 - Dada una direcci�n de archivo relativa, devuelve el contenidos del
 - archivo en una lista de bytes.
 -}
leerDeArchivo :: String -> IO [Byte]
leerDeArchivo archivo = BS.readFile archivo >>= return . BS.unpack

{-
 - Dada una direcci�n de archivo relativa y luego una lista de bytes,
 - imprimir dicha lista en el archivo.
 -}
imprimirArchivo :: String -> [Byte] -> IO ()
imprimirArchivo archivo = BS.writeFile archivo . BS.pack

{-
 - Transforma una lista de bits (booleanos) en una lista de bytes.
 -}
bitsABytes :: [Bool] -> [Byte]
bitsABytes lista = reverse $ bitsABytes' [] lista 0 0
	where
		bitsABytes' all []            acum 8 = acum:all
		bitsABytes' all []            acum n = bitsABytes' all        []    (acum*2)   (n+1)
		bitsABytes' all lista         acum 8 = bitsABytes' (acum:all) lista 0          0
		bitsABytes' all (False:lista) acum n = bitsABytes' all        lista (acum*2)   (n+1)
		bitsABytes' all (True:lista)  acum n = bitsABytes' all        lista (acum*2+1) (n+1)

{-
 - Transfirna un bite en una lista de bits (booleanos).
 -}
byteABits :: Byte -> [Bool]
byteABits = reverse . byteABits' 8
	where
		byteABits' 0 _ = []
		byteABits' n x
			| even x    = False : byteABits' (n-1) (div x 2)
			| otherwise = True  : byteABits' (n-1) (div x 2)

{-
 - Codifica un �rbol de Huffman en una representaci�n binaria.
 - Codificaci�n:
 - Si se lee un True, existen dos posibilidades. Si es el primer True que se lee
 - (ya sea al inicio del c�digo o despu�s de un False), entonces el siguiente bit
 - forma parte del �rbol (representado inversamente). De lo contrario, se da por
 - finalizado el c�digo. Si se lee un False, entonces los siguientes 8 bits,
 - corresponden a un s�mbolo.
 -}
codificarHuffman :: Huffman Byte -> [Bool]
codificarHuffman h = concat (map codificarAsoc asocs) ++ [True, False]
	where
		asocs = generarAsociaciones h
		codificarAsoc (e, cods)
			= (True : intersperse True (reverse cods)) ++ (False : byteABits e)

{-
 - Lee una codificaci�n de un �rbol de Huffman y reconstruye el
 - �rbol del que fue generado, separando el resto de los datos binarios.
 - (El �rbol de Huffman generado tendr� frecuencias acumuladas triviales,
 - iguales a cero).
 -}
parsearHuffman :: [Bool] -> (Huffman Byte, [Bool])
parsearHuffman datos = (huff, trim resto)
	where
		(asocs, resto) = parseAsocsInit [] datos
		huff           = reconstruirHuffman asocs
		trim           = reverse . tail . dropWhile not . reverse
		parseAsocsInit acum (True:False:xs) = (acum, xs)
		parseAsocsInit acum (True:True:xs)  = parseAsocs acum [True] xs
		parseAsocs acum cod (False:xs)      = parseAsocsInit ((head $ bitsABytes e, cod):acum) r
			where (e, r) = splitAt 8 xs
		parseAsocs acum cod (True:x:xs)     = parseAsocs acum (x:cod) xs

{-
 - Transforma una lista de byte en otra, codific�ndola o decodific�ndola seg�n
 - sea el caso (True es codificar, false es decodificar).
 - Al codificar, el �ltimo True se agrega a manera de tener un caracter de fin
 - de archivo. (Todos los False hacia el final de la cadena y el �ltimo True
 - ser�n luego ignorados por el decodificador).
 -}
transformar :: Bool -> [Byte] -> [Byte]
transformar True datos = bitsABytes $ codificarHuffman huff ++ cods ++ [True]
	where
		(huff, cods) = codificar datos
transformar _ datos = decodificar huff cods
	where
		(huff, cods) = parsearHuffman . concat $ map byteABits datos