{-|
Module      : Descifrador
Description : Implementacion del modo descifrador para Wordle que implementa un arbol minimax y adivina una palabra para el usuario.
Copyright   : Adelina Figueira
              Luis Garcia
License     : GPL-3
Maintainer  : 15-10484@usb.ve
Stability   : experimental
Portability : POSIX

Este modulo cuenta con las funciones 'revisar', 'stringAOpciones', 'mapearAOpciones', 'valorPalabra', 'conseguirValor', 
'valorPosibilidad', 'conseguirValorPosible', 'palabrasAUsar', 'conseguirPosibilidades', 'posibilidadAUsar', 'crearArbol', 
'crearHijosPalabras', 'crearHijosPosibilidades', 'recorrerArbol', 'recorrido', 'recorre', 'conseguirPalabras','palabraEncontrada'.
y 'conseguirW'.
La funciones 'revisar', 'stringAOpciones', 'mapearAOpciones', 'valorPalabra', 'conseguirValor', 
'valorPosibilidad', 'conseguirValorPosible', 'palabrasAUsar', 'conseguirPosibilidades' y 'posibilidadAUsar' son funciones auxiliares
que se utilizan para la creacion del arbol. 
Las funciones 'crearArbol', 'crearHijosPalabras' y 'crearHijosPosibilidades' se utilizan para la creacion del arbol minimax, con ayuda
de las funciones auxiliares anteriores.
Y las funciones 'recorrerArbol', 'recorrido', 'recorre' y 'conseguirPalabras' son utilizadas por la funcion 'palabraEncontrada' para 
recorrer el arbol y conseguir la palabra con el menor valor, que sera la adivinacion de la computadora.
-}
module Descifrador 
  ( -- * Importa el tipo 'Arbol' y las funciones 'stringAOpciones', 'crearArbol' y 'palabraEncontrada'
    -- | El tipo 'Arbol' representa al arbol minimax que se utiliza para que la computadora pueda adivinar la palabra
    -- seleccionada por el usuario. La funcion 'stringAOpciones' mapea un string al tipo 'Opciones', 'crearArbol' genera
    -- un arbol minimax con ayuda de todas las funciones que se encuentran en este modulo y 'palabraEncontrada' recorre el 
    -- arbol minimax y busca la palabra del nivel 1 con el valor menor.
   Arbol(Empty,Nodo)
  ,stringAOpciones
  ,crearArbol
  ,palabraEncontrada
  ) where

import MenteMaestra ( Opciones(Toro,Vaca,Vacio), conseguir )
import Data.Map ( fromListWith, Map )
import qualified Data.Map as Map
import System.Environment (getArgs)
import System.IO (hFlush,stdout)
import System.Random
import Data.Char (toUpper)
import Data.Ord
import Data.List


data Arbol a b c              
  =  Empty                      -- ^ El nodo Empty es el nodo de un padre que no tiene hijos
  |  Nodo                       -- ^ El Nodo a b c contiene un nodo o varios en la lista que puede ser Empty o una llamada recursiva.
         a                      -- ^ a es la palabra que se guarda como 'String' en caso de que el nodo sea un nodo palabra sino es ""
         b                      -- ^ b es la posibilidad que se guarda como 'Opciones' en caso de que sea un nodo posibilidad sino es []
         c                      -- ^ c representa el valor de la palabra o la posibilidad en 'Float'
         [ Arbol a b c]         -- ^ Esta es la llamada recursiva al tipo de dato, si no tiene hijos es Empty.
  deriving Show                 -- ^ derivado de 'Show' permite que el arbol pueda verse en la consola

{-|
  La funcion 'revisar' retorna una lista de 'String'. Anade a la lista de 'String' a aquellas palabras cuyo resultado con la funcion
  'conseguir' a aplicarla sobre la palabra y la palabra objetivo, es igual a lo que ha introducido el usuario.
  Toma tres argumentos de tipo '[String]', 'String' y '[Opciones]'.
-}
revisar :: [String] -> String -> [Opciones] -> [String]
revisar [] palabra guess = []
revisar (x:xs) palabra guess = 
                         if (conseguir x palabra) == guess
                            then [x] ++ revisar xs palabra guess
                         else
                            revisar xs palabra guess
                            
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
  Llama a la funcion conseguirW para conseguir la palabra de menor valor que no haya sido utilizada antes, para evitar la repeticion
  de palabras.
  Toma dos argumentos de tipo 'Arbol String [Opciones] Float' y '[String]'.
-}
palabraEncontrada :: Arbol String [Opciones] Float -> [String] -> [(String,Float)]
palabraEncontrada arbol palabrasGeneradas = conseguirW $ sortBy (comparing $ snd) (recorrerArbol arbol)
    where
        {-|
          La funcion 'palabraEncontrada' retorna '[(String,Float)]'. Itera sobre la lista de tuplas que recibe para encontrar
          a la palabra de menor valor que no se encuentra en la lista de palabrasGeneradas anteriormente por el programa.
          Toma un argumento de tipo '[(String,Float)]'.
        -}
        conseguirW :: [(String,Float)] -> [(String,Float)]
        conseguirW [] = []
        conseguirW (x:xs)
                        | (((fst x) `elem` palabrasGeneradas) == True) = conseguirW xs
                        | (((fst x) `elem` palabrasGeneradas) == False)= [x]
