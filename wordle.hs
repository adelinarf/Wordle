import Data.Map ( fromListWith, Map )
import qualified Data.Map as Map
import System.Environment (getArgs)
import System.IO (hFlush,stdout)
import System.Random
import Data.Char (toUpper)
import Data.Ord
import Data.List

data Opciones 
  = Toro                        -- ^ Toro es el tipo de dato que representa a los toros en el juego.
  | Vaca                        -- ^ Vaca es el tipo de dato que representa a las vacas en el juego.
  | Vacio                       -- ^ Vacio es el tipo de dato que representa a los valores que no son vacas ni toros.
  deriving (Eq)                 -- ^ Permite que se comparen los tipos de Opciones entre ellos con el operador ==

data Arbol a b c              
  =  Empty                      -- ^ El nodo Empty es el nodo de un padre que no tiene hijos
  |  Nodo                       -- ^ El Nodo a b c contiene un nodo o varios en la lista que puede ser Empty o una llamada recursiva.
         a                      -- ^ a es la palabra que se guarda como 'String' en caso de que el nodo sea un nodo palabra sino es ""
         b                      -- ^ b es la posibilidad que se guarda como 'Opciones' en caso de que sea un nodo posibilidad sino es []
         c                      -- ^ c representa el valor de la palabra o la posibilidad en 'Float'
         [ Arbol a b c]         -- ^ Esta es la llamada recursiva al tipo de dato, si no tiene hijos es Empty.
  deriving Show                 -- ^ derivado de 'Show' permite que el arbol pueda verse en la consola


instance Show Opciones where
-- ^ Esta instancia convierte en string al tipo Opciones. "T" para Toro, "V" para Vaca y "-" para Vacio.
    show Toro = "T"             
    show Vaca = "V"
    show Vacio = "-"

{-|
  La funcion 'p' retorna un String. Toma 'Opciones' y las transforma a 'String' con 'show' y los concatena en un unico string.
  Toma un argumento de tipo '[Opciones]'.
-}
p :: [Opciones] -> String
p (x:xs) = show x ++ p xs
p _ = ""

{-|
  La funcion 'g' retorna una tupla que contiene en su primer elemento una lista de 'Opciones' o 'Char' y en su segunda posicion un 
  diccionario. Utiliza dos funciones toroVaca para conseguir los toros en la palabra guess y crearDict para modificar una diccionario 
  que contiene la letra y cuantas veces aparece en la palabra guess. Se modifica para contar los toros conseguidos hasta el momento.
  Toma dos argumentos de tipo 'String'.
-}
g :: String -> String -> ([Either Opciones Char],Map Char Int)
g target guess = (zipWith toroVaca target guess, Prelude.foldr crearDict (fromListWith (+) [(c, 1) | c <- target]) (zip target guess))
    where
        {-|
          La funcion 'toroVaca' retorna el tipo 'Either Opciones Char' para verificar si dos caracteres son iguales o no. Si lo son es Toro
          sino retorna el caracter.
          Toma dos argumentos de tipo 'Char'.
        -}
        toroVaca :: Char -> Char -> Either Opciones Char
        toroVaca x y
            | x == y = Left Toro
            | otherwise = Right y
        {-|
          La funcion 'crearDict' retorna un diccionario de caracteres y enteros. Esta funcion toma una tupla de characteres y si son 
          iguales resta el valor en el diccionario, ya que son toros.
          Toma dos argumentos de tipo '(Char,Char)' y 'Map Char Int'.
        -}
        crearDict :: (Char,Char) -> Map Char Int -> Map Char Int
        crearDict (t,g) dict =
             if t==g
                 then Map.adjust pred t dict
                 else dict

{-|
  La funcion 'h' retorna una lista de 'Opciones'. Esta funcion utiliza la funcion buscar para revisar cada elemento de una lista de 
  'Opciones' o 'Char' y verificar si tiene Vacas o Vacio. Como buscar retorna una tupla, la funcion 'h' toma el primer elemento 
  y lo retorna.
  Toma dos argumentos de tipo '[Either Opciones Char]' y 'Map Char Int'.
-}
h :: [Either Opciones Char] -> Map Char Int -> [Opciones]
h lista diccionario = fst (Prelude.foldl buscar ([],diccionario) lista)
    where
        {-|
          La funcion 'buscar' retorna una tupla cuyo primer elemento es una lista de 'Opciones' y su segundo elemento es un diccionario.
          Esta funcion guarda en una lista los valores de la lista de Opciones, como se llama con la funcion foldl, retorna esta lista
          para ser utilizada por la siguiente posicion. Si la entrada esta en Opciones, es un toro y se agrega a la lista de Opciones
          si es un caracter se verifica si esta en el diccionario. Si esta, se disminuye su valor y se guarda como Vaca, si no pertenece
          se guarda como Vacio.
          Toma dos argumentos de tipo '([Opciones],Map Char Int)' y 'Either Opciones Char'.
        -}
        buscar :: ([Opciones],Map Char Int) -> Either Opciones Char ->  ([Opciones],Map Char Int)
        buscar (ansSoFar,mem) (Left opcion)  = ( ansSoFar ++ [opcion],mem)
        buscar (ansSoFar,mem) (Right palabra) =
            if Map.lookup palabra mem > Just 0
                then (  ansSoFar ++ [Vaca],Map.adjust pred palabra mem)
                else ( ansSoFar ++ [Vacio],mem)

{-|
  La funcion 'conseguir' retorna una lista de 'Opciones'. Aplica las funciones g y h a las palabras objetivo y la introducida por el usuario.
  Toma dos argumentos de tipo 'String'.
-}
conseguir :: String -> String -> [Opciones]
conseguir target = uncurry h . g target





palabras :: IO [String]
palabras = do
    t <- readFile "Palabras.txt"
    let palabras = lines t
    return palabras

--FUNCIONES MODO MENTE MAESTRA
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- //////////////////////////////////////////////////////////////////////////////////////////////////////////
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- //////////////////////////////////////////////////////////////////////////////////////////////////////////
modo1Init :: IO ()
modo1Init = do
    texto <-  readFile "Palabras.txt"
    p <- palabras
    i <- randomRIO (0, length p - 1)
    let target = p !! i
    modo1 target [Vacio | x<-[1..5]] [] 6

modo1 :: [Char] -> [Opciones] -> [[Opciones]] -> Int -> IO ()
modo1 target prevRes log lives
    | prevRes == [Toro | x<-[1..5]] = modo1End "Ganaste!" log
    | lives == 0 = modo1End ("La palabra era: " ++ target) log
    | otherwise = do
        putStr "DECIFRADOR  : "
        hFlush stdout
        guess <- getLine
        p <- palabras
        validate target log lives (Prelude.map (convertV . toUpper) guess) p

convertV :: Char -> Char
convertV x
    | x == 'Á' = 'A'
    | x == 'É' = 'E'
    | x == 'Í' = 'I'
    | x == 'Ó' = 'O'
    | x == 'Ú' = 'U'
    | x == 'Ú' = 'U'
    | otherwise = x

validate :: [Char] -> [[Opciones]] -> Int -> [Char] -> [String]-> IO ()
validate target log lives guess palabras
    | 'Ñ' `elem` guess || length guess /= 5 ||  guess `notElem` palabras = do
        putStrLn "Entrada invalida"
        modo1 target [] log  lives
    | otherwise = do
        let res =  target `conseguir`  guess
        putStrLn $ "MENTEMAESTRA: " ++ p res
        modo1 target res (res:log)  (lives-1)


printLog :: [[Opciones]] -> IO ()
printLog [] = putStrLn ""
printLog [x] = putStrLn $ p x
printLog (x:xs) = do
    printLog xs
    putStrLn $ p x

modo1End :: [Char] -> [[Opciones]] -> IO ()
modo1End msg log = do
    putStrLn $ "\n" ++ msg
    putStrLn "\nComparte tu resultado:"
    printLog log





--FUNCIONES PARA MODO DESCIFRADOR
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- //////////////////////////////////////////////////////////////////////////////////////////////////////////
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- //////////////////////////////////////////////////////////////////////////////////////////////////////////

{-|
  La funcion 'agregarPalabra' retorna una lista de 'String'. Concatena un 'String' en un '[String]'
  Toma dos argumentos de tipo 'String' y '[String]'.
-}
agregarPalabra :: String -> [String] -> [String]
agregarPalabra a xs = xs ++ [a]

{-|
  La funcion 'revisar' retorna una lista de 'String'. Anade a la lista de 'String' a aquellas palabras cuyo resultado con la funcion
  'conseguir' a aplicarla sobre la palabra y la palabra objetivo, es igual a lo que ha introducido el usuario.
  Toma tres argumentos de tipo '[String]', 'String' y '[Opciones]'.
-}
revisar :: [String] -> String -> [Opciones] -> [String]
revisar [] palabra guess = []
revisar (x:xs) palabra guess = 
                         if conseguir palabra x == guess
                            then [x] ++ revisar xs palabra guess
                         else
                            revisar xs palabra guess
                            
--posibilidades retorna las posibilidades existentes en una lista de listas
posibilidades :: Int -> [[Opciones]]
posibilidades x = [[a,b,c,d,e] | a <- [Toro,Vaca,Vacio], b <- [Toro,Vaca,Vacio],c <- [Toro,Vaca,Vacio],d <- [Toro,Vaca,Vacio],e<- [Toro,Vaca,Vacio]]

{-|
  La funcion 'stringAOpciones' retorna 'Opciones'.
  Toma un argumento de tipo 'Char'.
-}
stringAOpciones :: Char -> Opciones
stringAOpciones 'T' = Toro
stringAOpciones 'V' = Vaca
stringAOpciones 't' = Toro
stringAOpciones 'v' = Vaca
stringAOpciones '-' = Vacio
stringAOpciones entrada = Vacio

{-|
  La funcion 'mapearAOpciones' retorna '[Opciones]'. Mapea letras a elementos del tipo 'Opciones'.
  Toma un argumento de tipo 'String'.
-}
mapearAOpciones :: String -> [Opciones]
mapearAOpciones palabra = Prelude.map stringAOpciones palabra 

{-|
  La funcion 'valorPalabra' retorna 'Float'. Consigue el valor de una palabra, consigue el valor de cada letra con la funcion
  'conseguirValor' y luego suma todos los valores de la lista.
  Toma un argumento de tipo 'String'.
-}
valorPalabra :: String -> Float
valorPalabra palabra = sum (Prelude.map conseguirValor palabra)

{-|
  La funcion 'conseguirValor' retorna 'Float'. Se verifica que la letra sea parte del conjunto que se desea.
  Toma un argumento de tipo 'Char'.
-}
conseguirValor :: Char -> Float
conseguirValor letra
    | letra `elem` ['A','E'] = 0.1
    | letra `elem` ['I','N','O','R','S'] = 0.2
    | letra `elem` ['D','L','C','T','U'] = 0.3
    | letra `elem` ['B','G','M','P'] = 0.5
    | letra `elem` ['F','H','Q','V','Y'] = 0.8
    | otherwise = 1

{-|
  La funcion 'valorPosibilidad' retorna 'Float'. Consigue el valor de cada una de las 'Opciones' de una lista y los suma.
  Toma un argumento de tipo '[Opciones]'.
-}
valorPosibilidad :: [Opciones] -> Float
valorPosibilidad posibilidad = 1.0 + sum (Prelude.map conseguirValorPosible posibilidad)

{-|
  La funcion 'conseguirValorPosible' retorna 'Float'. Retorna -0.2 para Toro, -0.1 para Vaca y 0 si es Vacio.
  Toma un argumento de tipo 'Opciones'.
-}
conseguirValorPosible :: Opciones -> Float
conseguirValorPosible opcion
    | opcion == Toro = -0.2
    | opcion == Vaca = -0.1
    | otherwise = 0

{-|
  La funcion 'palabrasAUsar' retorna '[(String,Float)]'. Esta funcion genera una lista de tuplas que toma las lista de palabras de la
  funcion 'revisar' y busca el valor de cada una de las palabras con 'valorPalabra'.
  Toma un argumento de tipo '[String]'.
-}
palabrasAUsar :: [String] -> [(String,Float)]
palabrasAUsar [] = []
palabrasAUsar (x:xs) = [(x,valorPalabra x)] ++ palabrasAUsar xs

{-|
  La funcion 'conseguirPosibilidades' retorna '[[Opciones]]'. Esta funcion genera una lista de de listas de 'Opciones'. Las listas de
  'Opciones' se obtienen con la funcion 'conseguir' sobre cada palabra y la palabra objetivo.
  Toma dos argumentos de tipo '[String]' y 'String'.
-}
conseguirPosibilidades :: [String] -> String -> [[Opciones]]
conseguirPosibilidades [] palabra = []
conseguirPosibilidades (x:xs) palabra = [conseguir palabra x] ++ conseguirPosibilidades xs palabra

{-|
  La funcion 'posibilidadAUsar' retorna '[([Opciones],Float)]'. Esta funcion toma la lista de listas de 'Opciones' de la funcion
  'conseguirPosibilidades' y busca su valor con 'valorPosibilidad' y lo anade en una tupla y en una lista de tuplas.
  Toma un argumento de tipo '[[Opciones]]'.
-}
posibilidadAUsar :: [[Opciones]] -> [([Opciones],Float)]
posibilidadAUsar [] = []
posibilidadAUsar (x:xs) = [(x,valorPosibilidad x)] ++ posibilidadAUsar xs

{-|
  La funcion 'crearArbol' retorna 'Arbol String [Opciones] Float'. Crear un nodo padre y llama a la funcion 'crearHijosPalabras' para
  crear el resto del arbol.
  Toma tres argumentos de tipo '[Opciones]', 'String' y '[String]'.
-}
crearArbol :: [Opciones] -> String -> [String] -> Arbol String [Opciones] Float
crearArbol opciones palabra listaPalabras = Nodo "" opciones 0.0 ([] ++ crearHijosPalabras opciones palabra listaPalabras 1)

{-|
  La funcion 'crearHijosPalabras' retorna '[Arbol String [Opciones] Float]'. Crear los nodos hijos que son palabras, conseguidas por 
  medio de la funcion 'palabrasAUsar' y toma unicamente los primeros 10 de menor valor. Llama a la funcion 'transform' que se encarga
  de crear el parte del arbol y llama recursivamente a 'crearHijoPosibilidades'.
  Toma cuatro argumentos de tipo '[Opciones]', 'String', '[String]' y 'Int'.
-}
crearHijosPalabras :: [Opciones] -> String -> [String] -> Int -> [Arbol String [Opciones] Float]
crearHijosPalabras opciones palabra listaPalabrasCompleta numero = transform (Prelude.take 10 (sortBy (comparing $ snd) (palabrasAUsar (revisar listaPalabrasCompleta palabra opciones)))) numero
    where
        {-|
          La funcion 'transform' retorna '[Arbol String [Opciones] Float]'. Y crea los hijos palabras del arbol y considera una variable
          num para verificar el nivel del arbol en el que se encuentra, si esta en el ultimo, anade como hijo a un nodo Empty para 
          finalizar. Llama a 'crearHijosPosibilidades' para seguir creando el resto del arbol. Y verifica los hijos palabras seleccionados
          antes de llamar a la funcion, para cada uno de ellos crea 10 hijos a lo sumo.
          Toma dos argumentos de tipo '[(String,Float)]' y Int'.
        -}
        transform :: [(String,Float)] -> Int -> [Arbol String [Opciones] Float]
        transform algo 4 = [Empty]
        transform [] num = []
        transform (x:xs) num = [Nodo (fst x) [] (snd x) (crearHijosPosibilidades (fst x) listaPalabrasCompleta (num+1))] ++ transform xs num

{-|
  La funcion 'crearHijosPosibilidades' retorna '[Arbol String [Opciones] Float]'. Crear los nodos hijos que son posibilidades, 
  conseguidas por medio de la funcion 'posibilidadAUsar' y toma unicamente los ultimos 10 de mayor valor. Llama a la funcion 'transformar'
  que se encarga de crear parte del arbol y llama recursivamente a 'crearHijosPalabras'.
  Toma tres argumentos de tipo 'String', '[String]' y 'Int'.
-}
crearHijosPosibilidades :: String -> [String] -> Int -> [Arbol String [Opciones] Float]              
crearHijosPosibilidades palabra listaPalabras numero = transformar (Prelude.take 10 $ sortBy (comparing $ Down . snd) $ posibilidadAUsar (conseguirPosibilidades listaPalabras palabra)) numero
    where
        {-|
          La funcion 'transformar' retorna '[Arbol String [Opciones] Float]'. Y crea los hijos posibilidades del arbol y considera una 
          variable num para verificar el nivel del arbol en el que se encuentra, si esta en el ultimo, anade como hijo a un nodo Empty para 
          finalizar. Llama a 'crearHijosPalabras' para seguir creando el resto del arbol. Y verifica los hijos posibilidades seleccionados
          antes de llamar a la funcion, para cada uno de ellos crea 10 hijos a lo sumo.
          Toma dos argumentos de tipo '[([Opciones],Float)]' y Int'.
        -}
        transformar :: [([Opciones],Float)] -> Int -> [Arbol String [Opciones] Float]
        transformar algo 4 = [Empty]
        transformar [] num = []
        transformar (x:xs) num = [Nodo "" (fst x) (snd x) (crearHijosPalabras (fst x) palabra listaPalabras (num+1))] ++ transformar xs num


--FUNCIONES PARA RECORRER EL ARBOL
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- //////////////////////////////////////////////////////////////////////////////////////////////////////////
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- //////////////////////////////////////////////////////////////////////////////////////////////////////////

{-|
  La funcion 'recorrerArbol' retorna '[(String,Float)]'. Recorre el arbol con la funcion 'recorrido' para conseguir los valores de
  cada nodo hijo palabra y con zip 'conseguirPalabras' consigue el nombre de los nodos y lo une en una tupla.
  Toma un argumento de tipo 'Arbol String [Opciones] Float'.
-}
recorrerArbol :: Arbol String [Opciones] Float -> [(String,Float)]
recorrerArbol (Nodo palabra opciones valor [Empty]) = [(palabra,valor)]
recorrerArbol (Nodo palabra opciones valor arbolHijo) = zip (conseguirPalabras arbolHijo) (Prelude.map recorrido arbolHijo)

{-|
  La funcion 'recorrido' retorna 'Float'. Toma un arbol y empieza a recorrerlo para sumar su valor y llama a la funcion 'recorrer'
  para recorrer al hijo que se encuentra en una lista. Si su hijo es 'Empty' retorna solo el valor.
  Toma un argumento de tipo 'Arbol String [Opciones] Float'.
-}
recorrido :: Arbol String [Opciones] Float -> Float
recorrido (Nodo palabra opciones valor [Empty]) = valor
recorrido (Nodo palabra opciones valor arbolHijo) = valor + recorre arbolHijo

{-|
  La funcion 'recorre' retorna 'Float'. Toma una lista de arboles y empieza a recorrerlos con ayuda de la funcion 'recorrido' que toma
  el arbol como argumento.
  Toma un argumento de tipo '[Arbol String [Opciones] Float]'.
-}
recorre :: [Arbol String [Opciones] Float] -> Float
recorre [] = 0
recorre (x:xs) = recorrido x + recorre xs

{-|
  La funcion 'conseguirPalabras' retorna '[String]'. Toma los nodos del primer nivel y guarda las palabras en una lista de 'String'.
  Toma un argumento de tipo '[Arbol String [Opciones] Float]'.
-}
conseguirPalabras :: [Arbol String [Opciones] Float] -> [String]
conseguirPalabras [] = []
conseguirPalabras ((Nodo palabra opciones valor arbolHijo):xs) = [palabra] ++ conseguirPalabras xs

{-|
  La funcion 'palabraEncontrada' retorna '[(String,Float)]'. Recorre el arbol y toma la palabra del primer nivel de menor valor.
  Toma un argumento de tipo 'Arbol String [Opciones] Float'.
-}
palabraEncontrada :: Arbol String [Opciones] Float -> [(String,Float)]
palabraEncontrada arbol = Prelude.take 1 $ sortBy (comparing $ snd) (recorrerArbol arbol)




--FUNCIONES MODO DE JUEGO DESCIFRADOR ENTRADAS
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- //////////////////////////////////////////////////////////////////////////////////////////////////////////
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- //////////////////////////////////////////////////////////////////////////////////////////////////////////
modo2End :: Int -> IO ()
modo2End 0 = putStrLn "La entrada solo debe contener, V, T y -"
modo2End 1 = putStrLn "Tramposo!"
modo2End 2 = putStrLn "Ganaste!"
modo2End 3 = putStrLn "La computadora ha ganado!"

modo2Init :: IO ()
modo2Init = do
    listaPalabras <- palabras
    i <- randomRIO (0, length listaPalabras - 1)
    let target = listaPalabras !! i 
    modo2 listaPalabras target 6

modo2 :: [String] -> String -> Int -> IO ()
modo2 listaPalabras target vidas
    | vidas == 0 = modo2End 2
    | otherwise = do
        putStr ("DESCIFRADOR  : " ++ target ++ "\n")
        hFlush stdout
        putStr "MENTE MAESTRA  : "
        hFlush stdout
        input <- getLine
        if (all (\x -> x `elem` ['T','V','-']) input) == False || length input /= 5
            then modo2End 0
        else do
            let guess = listaOpciones input
            if guess == [Toro,Toro,Toro,Toro,Toro]
                then modo2End 3
            else do
                validarE guess listaPalabras target vidas
        

listaOpciones :: String -> [Opciones]
listaOpciones entrada = Prelude.map stringAOpciones entrada

validarE :: [Opciones] -> [String] -> String -> Int -> IO ()
validarE guess listaPalabras palabra vidas
    | length guess /= 5 = do
        putStrLn "Entrada invalida"
        modo2 listaPalabras palabra vidas
    | otherwise = do
        let arbol = crearArbol guess palabra listaPalabras
        let tupla = palabraEncontrada arbol
        if length tupla == 0 
           then modo2End 1
        else do
           modo2 listaPalabras (fst (tupla !! 0)) (vidas-1)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Es necesario el argumento de modo de juego"
        [x]-> case  x of
            "mentemaestra" -> modo1Init
            "descifrador" -> modo2Init
            _ -> putStrLn "Argumento invalido"
        _-> putStrLn $ "Comando invalido: " ++ args !! 1
