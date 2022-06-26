import Data.Map
import Data.Ord
import Data.List
import qualified Data.Map as Map

data Opciones = Toro | Vaca | Vacio deriving (Show,Eq)

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

--Se elige una palabra al azar y se crea un arbol con:
--      crearArbol inputUsuarioComoListaOpciones palabraAlAzar listaPalabras
--La lista de palabras se puede conseguir como 
--x <- readFile "Palabras.txt"
--listaPalabras = lines x
--Se deben considerar los casos cuando no llegan a 10 ni la lista de posibilidades o palabras en caso de que ocurra, para que no haya error
--en el arbol
--Y falta un algoritmo que visite al arbol y sume los valores alojados en c para ver cual es la palabra que escoge 
--Convertir todo lo que introduzca el usuario en mayusculas si no lo son

