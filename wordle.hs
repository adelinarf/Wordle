import Data.Map ( fromListWith, Map )
import qualified Data.Map as Map
import System.Environment (getArgs)
import System.IO (hFlush,stdout)
import System.Random
import Data.Char (toUpper)
import Data.Ord
import Data.List

data Opciones = Toro | Vaca | Vacio deriving (Eq)

instance Show Opciones where
    show Toro = "T"
    show Vaca = "V"
    show Vacio = "-"

p :: [Opciones] -> String
p (x:xs) = show x ++ p xs
p _ = ""


g :: String -> String -> ([Either Opciones Char],Map Char Int)
g target guess = (zipWith toroVaca target guess, Prelude.foldr crearDict (fromListWith (+) [(c, 1) | c <- target]) (zip target guess))
    where
        toroVaca :: Char -> Char -> Either Opciones Char
        toroVaca x y
            | x == y = Left Toro
            | otherwise = Right y
        crearDict :: (Char,Char) -> Map Char Int -> Map Char Int
        crearDict (t,g) dict =
             if t==g
                 then Map.adjust pred t dict
                 else dict

h :: [Either Opciones Char] -> Map Char Int -> [Opciones]
h lista diccionario = fst (Prelude.foldl buscar ([],diccionario) lista)
    where
        buscar :: ([Opciones],Map Char Int) -> Either Opciones Char ->  ([Opciones],Map Char Int)
        buscar (ansSoFar,mem) (Left opcion)  = ( ansSoFar ++ [opcion],mem)
        buscar (ansSoFar,mem) (Right palabra) =
            if Map.lookup palabra mem > Just 0
                then (  ansSoFar ++ [Vaca],Map.adjust pred palabra mem)
                else ( ansSoFar ++ [Vacio],mem)

--verificar que la letra este en el diccionario
-- que sea mayor que 0
-- si cumple retorna V

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
    | prevRes == [Toro | x<-[1..5]] = modo1End "¡Ganaste!" log
    | lives == 0 = modo1End ("La palabra era " ++ target) log
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
        putStrLn "entrada inválida"
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
agregarPalabra :: String -> [String] -> [String]
agregarPalabra a xs = xs ++ [a]

--Entra con la lista de palabras, una palabra que se quiere conseguir y el guess del usuario
--Con revisar se consigue la lista de palabra que cumple con un set de opciones dados por el usuario
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

--transformar un string a toros y vacas
--funcion para mapear
stringAOpciones :: Char -> Opciones
stringAOpciones 'T' = Toro
stringAOpciones 'V' = Vaca
stringAOpciones '-' = Vacio
stringAOpciones entrada = Vacio

mapearAOpciones :: String -> [Opciones]
mapearAOpciones palabra = Prelude.map stringAOpciones palabra 

--conseguir el valor de una palabra
valorPalabra :: String -> Float
valorPalabra palabra = sum (Prelude.map conseguirValor palabra)

conseguirValor :: Char -> Float
conseguirValor letra --INCLUIR LAS MINUSCULAS
    | letra `elem` ['A','E'] = 0.1
    | letra `elem` ['I','N','O','R','S'] = 0.2
    | letra `elem` ['D','L','C','T','U'] = 0.3
    | letra `elem` ['B','G','M','P'] = 0.5
    | letra `elem` ['F','H','Q','V','Y'] = 0.8
    | otherwise = 1

--conseguir el valor de una posibilidad
--A cada posibilidad se le asigna 1 punto, y se restan 0,2 puntos por cada toro y 0,1 puntos por cada vaca

valorPosibilidad :: [Opciones] -> Float
valorPosibilidad posibilidad = 1.0 + sum (Prelude.map conseguirValorPosible posibilidad)

conseguirValorPosible :: Opciones -> Float
conseguirValorPosible opcion
    | opcion == Toro = -0.2
    | opcion == Vaca = -0.1
    | otherwise = 0

palabrasAUsar :: [String] -> [(String,Float)]
palabrasAUsar [] = []
palabrasAUsar (x:xs) = [(x,valorPalabra x)] ++ palabrasAUsar xs

--Entra con la lista de palabras, una palabra que se quiere conseguir
conseguirPosibilidades :: [String] -> String -> [[Opciones]]
conseguirPosibilidades [] palabra = []
conseguirPosibilidades (x:xs) palabra = [conseguir palabra x] ++ conseguirPosibilidades xs palabra

posibilidadAUsar :: [[Opciones]] -> [([Opciones],Float)]
posibilidadAUsar [] = []
posibilidadAUsar (x:xs) = [(x,valorPosibilidad x)] ++ posibilidadAUsar xs

--se debe conseguir de palabrasAUsar las 10 primeras de menor valor y de posibilidadAUsar las 10 primeras de mayor valor

data Arbol a b c =  Empty  |  Nodo a b c [ Arbol a b c] deriving Show
--a es el nombre de la palabra, b es la posibilidad y c es el puntaje
crearArbol :: [Opciones] -> String -> [String] -> Arbol String [Opciones] Float
crearArbol opciones palabra listaPalabras = Nodo "" opciones 0.0 ([] ++ crearHijosPalabras opciones palabra listaPalabras 1)

--lista de tuplas -> lista de arboles
crearHijosPalabras :: [Opciones] -> String -> [String] -> Int -> [Arbol String [Opciones] Float]
crearHijosPalabras opciones palabra listaPalabrasCompleta numero = transform (Prelude.take 10 (sortBy (comparing $ snd) (palabrasAUsar (revisar listaPalabrasCompleta palabra opciones)))) numero
    where
        transform :: [(String,Float)] -> Int -> [Arbol String [Opciones] Float]
        transform algo 4 = [Empty]
        transform [] num = []
        transform (x:xs) num = [Nodo (fst x) [] (snd x) (crearHijosPosibilidades (fst x) listaPalabrasCompleta (num+1))] ++ transform xs num

crearHijosPosibilidades :: String -> [String] -> Int -> [Arbol String [Opciones] Float]              
crearHijosPosibilidades palabra listaPalabras numero = transformar (Prelude.take 10 $ sortBy (comparing $ Down . snd) $ posibilidadAUsar (conseguirPosibilidades listaPalabras palabra)) numero
    where
        transformar :: [([Opciones],Float)] -> Int -> [Arbol String [Opciones] Float]
        transformar algo 4 = [Empty]
        transformar [] num = []
        transformar (x:xs) num = [Nodo "" (fst x) (snd x) (crearHijosPalabras (fst x) palabra listaPalabras (num+1))] ++ transformar xs num

--FUNCIONES PARA RECORRER EL ARBOL
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- //////////////////////////////////////////////////////////////////////////////////////////////////////////
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- //////////////////////////////////////////////////////////////////////////////////////////////////////////
recorrerArbol :: Arbol String [Opciones] Float -> [(String,Float)]
recorrerArbol (Nodo palabra opciones valor [Empty]) = [(palabra,valor)]
recorrerArbol (Nodo palabra opciones valor arbolHijo) = zip (conseguirPalabras arbolHijo) (Prelude.map recorrido arbolHijo)

recorrido :: Arbol String [Opciones] Float -> Float
recorrido (Nodo palabra opciones valor [Empty]) = valor
recorrido (Nodo palabra opciones valor arbolHijo) = valor + recorre arbolHijo

recorre :: [Arbol String [Opciones] Float] -> Float
recorre [] = 0
recorre (x:xs) = recorrido x + recorre xs

conseguirPalabras :: [Arbol String [Opciones] Float] -> [String]
conseguirPalabras [] = []
conseguirPalabras ((Nodo palabra opciones valor arbolHijo):xs) = [palabra] ++ conseguirPalabras xs


palabraEncontrada :: Arbol String [Opciones] Float -> [(String,Float)]
palabraEncontrada arbol = Prelude.take 1 $ sortBy (comparing $ snd) (recorrerArbol arbol)

--FUNCIONES MODO DE JUEGO DESCIFRADOR ENTRADAS
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- //////////////////////////////////////////////////////////////////////////////////////////////////////////
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- //////////////////////////////////////////////////////////////////////////////////////////////////////////
modo2End :: Int -> IO ()
modo2End 0 = putStrLn "La entrada solo debe contener, V, T y -"
modo2End 1 = putStrLn "¡Tramposo!"
modo2End 2 = putStrLn "¡Ganaste!"

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
            validarE guess listaPalabras target vidas
        

listaOpciones :: String -> [Opciones]
listaOpciones entrada = Prelude.map stringAOpciones entrada

validarE :: [Opciones] -> [String] -> String -> Int -> IO ()
validarE guess listaPalabras palabra vidas
    | length guess /= 5 = do
        putStrLn "Entrada inválida"
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
        _-> putStrLn $ "Comando inválido: " ++ args !! 1
