{-|
Module      : MenteMaestra
Description : Implementacion del modo mente maestra para Wordle que busca la lista 'Opciones' para dos palabras dadas.
Copyright   : Adelina Figueira
              Luis Garcia
License     : GPL-3
Maintainer  : 15-10484@usb.ve
Stability   : experimental
Portability : POSIX

Este modulo cuenta con las funciones 'g', 'h' y 'conseguir' que se encargan de visitar a las palabras dadas como tipo 'String'
y verificar los toros y vacas que posee cada unas de ellas. 
-}
module MenteMaestra 
  ( -- * Importa el tipo 'Opciones' y la funcion 'conseguir'
    -- | El tipo 'Opciones' representa los toros y vacas del juego y con la funcion conseguir se puede encontrar la combinacion
    -- de toros, vacas y vacio, dada una palabra objetivo y la palabra insertada por el usuario.
    Opciones(Toro,Vaca,Vacio)
  , conseguir 
  ) where

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
