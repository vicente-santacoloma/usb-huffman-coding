import System.Environment
import CompresorDeArchivos

aBooleano "codificar" = True
aBooleano "decodificar" = False
aBooleano _ = error "Mala invocacion de argumentos"

main = do
  args <- getArgs
  string <- leerDeArchivo(head args)
  imprimirArchivo (head  $ tail args) (transformar (aBooleano $ last args) string )