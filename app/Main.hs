{-|
Module      : Main
Description : Implementacion del juego Wordle en el lenguaje de programacion Haskell.
Copyright   : Adelina Figueira
              Luis Garcia
License     : GPL-3
Maintainer  : 15-10484@usb.ve
Stability   : experimental
Portability : POSIX

Este modulo cuenta con las funciones que se encargan del manejo de las entradas por consola y llama a las funciones de los modulos
MenteMaesta y Descifrador para procesar los datos insertados por el usuario. Ademas lee las palabras que se encuentran en el archivo
Palabras.txt y separa los modos mentemaestra y descifrador gracias a entradas por consola que son necesarias para el inicio de cada modo.
MODO MENTE MAESTRA: En este modo el jugador debe introducir una palabra y la computadora analiza cuales son toros y vacas, debido a que
ya ha escogido previamente una palabra que debe ser adivinada por el jugador.
MODO DESCIFRADOR: En este modo el jugador debe pensar una palabra de 5 letras y la computadora adivinara cual es la palabra del jugador.
La computadora muestra opciones de palabras y el jugador debe introducir los toros, vacas y vacios de esta palabra elegida para que la 
computadora pueda adivinar cual es la palabra, con ayuda de un arbol minimax.
En la funcion main se reciben argumentos por consola siendo: mentemaestra y descifrador los unicos argumentos validos para el juego. 
-}
module Main where

import MenteMaestra ( Opciones(Toro,Vaca,Vacio), conseguir )

import Descifrador  ( Arbol(Empty,Nodo),stringAOpciones,crearArbol,palabraEncontrada)

import Data.Map ( fromListWith, Map )
import qualified Data.Map as Map
import System.Environment (getArgs)
import System.IO (hFlush,stdout)
import System.Random
import Data.Char (toUpper)
import Data.Ord
import Data.List

instance Show Opciones where
-- ^ Esta instancia convierte en 'String' al tipo 'Opciones'. "T" para Toro, "V" para Vaca y "-" para Vacio.
    show Toro = "T"             
    show Vaca = "V"
    show Vacio = "-"

{-|
  La funcion 'p' retorna un 'String'. Toma 'Opciones' y las transforma a 'String' con 'show' y los concatena en un unico string.
  Toma un argumento de tipo '[Opciones]'.
-}
p :: [Opciones] -> String
p (x:xs) = show x ++ p xs
p _ = ""

{-|
  La funcion 'palabras' retorna un 'IO [String]'. Lee las palabras en el archivo Palabras.txt y las guarda en un 'IO [String]'.
  No toma argumentos de entrada.
-}
palabras :: IO [String]
palabras = do
    t <- readFile "Palabras.txt"
    let palabras = lines t
    return palabras

{-|
  La funcion 'modo1Init' retorna un 'I0 ()'. Toma una lista de las palabras en Palabras.txt y selecciona una al azar. Luego llama a
  la funcion 'modo1'.
  No toma ningun argumento.
-}
modo1Init :: IO ()
modo1Init = do
    texto <-  readFile "Palabras.txt"
    p <- palabras
    i <- randomRIO (0, length p - 1)
    let target = p !! i
    modo1 target [Vacio | x<-[1..5]] [] 6

{-|
  La funcion 'modo1' retorna un 'I0 ()'. Toma una palabra objetivo, un resultado de tipo '[Opciones]', aloja lo obtenido en una lista 
  de lista de 'Opciones' y un entero que define las oportunidades o vidas que quedan. Si el resultado '[Opciones]' contiene solo toros
  el jugador gana. Si ya no quedan oportunidades se muestra la palabra objetivo. Si aun quedan vidas y no se ha adivinado la palabra, se 
  toma la entrada por consola del usuario y se valida con la funcion 'validate'. Tambien se utiliza una funcion 'convertV' para eliminar
  los acentos de las palabras.
  Toma cuatro argumentos de tipo '[Char]', '[Opciones]', '[[Opciones]]' y 'Int'.
-}
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

{-|
  La funcion 'convertV' retorna un 'Char'. Mapea un char con acento al mismo char sin el acento.
  Toma un argumento de tipo 'Char'.
-}
convertV :: Char -> Char
convertV x
    | x == 'Á' = 'A'
    | x == 'É' = 'E'
    | x == 'Í' = 'I'
    | x == 'Ó' = 'O'
    | x == 'Ú' = 'U'
    | x == 'Ú' = 'U'
    | otherwise = x

{-|
  La funcion 'validate' retorna un 'IO ()'. Esta funcion toma una palabra y verifica que no tenga Ñ, que su tamano sea el
  correcto y se encuentre en la lista de palabras en Palabras.txt. Si lo introducido por el usuario cumple con todos estos
  requisitos, se calculan sus toros y vacas con la funcion 'conseguir' y la palabra objetivo. Ademas se guarda la lista de 
  'Opciones' en un log para despues mostrarla al usuario al finalizar la partida.
  Toma cinco argumentos de tipo '[Char]', '[[Opciones]]', 'Int', '[Char]' y '[String]'.
-}
validate :: [Char] -> [[Opciones]] -> Int -> [Char] -> [String]-> IO ()
validate target log lives guess palabras
    | 'Ñ' `elem` guess || length guess /= 5 ||  guess `notElem` palabras = do
        putStrLn "Entrada invalida"
        modo1 target [] log  lives
    | otherwise = do
        let res =  target `conseguir`  guess
        putStrLn $ "MENTEMAESTRA: " ++ p res
        modo1 target res (res:log)  (lives-1)

{-|
  La funcion 'printLog' retorna un 'IO ()'. Esta funcion imprime en la consola una lista de listas de 'Opciones' que contiene los 
  resultados de las evaluaciones de todas las palabras introducidas por el usuario con la funcion 'conseguir' y la palabra objetivo.
  Utiliza la funcion 'p' para conseguir el valor de las 'Opciones' como 'String' y poder mostrarlo en pantalla.  
  Toma un argumento de tipo '[[Opciones]]'.
-}
printLog :: [[Opciones]] -> IO ()
printLog [] = putStrLn ""
printLog [x] = putStrLn $ p x
printLog (x:xs) = do
    printLog xs
    putStrLn $ p x

{-|
  La funcion 'modo1End' retorna un 'IO ()'. Esta funcion se llama al finalizar una partida en modo mente maestra. Se encarga de mostrar
  en consola el resultado de las adivinaciones del usuario. Utiliza la funcion 'printLog' para mostrar la lista de '[Opciones]' de 
  cada adivinacion.  
  Toma dos argumentos de tipo '[Char]' y '[[Opciones]]'.
-}
modo1End :: [Char] -> [[Opciones]] -> IO ()
modo1End msg log = do
    putStrLn $ "\n" ++ msg
    putStrLn "\nComparte tu resultado:"
    printLog log

{-|
  La funcion 'modo2End' retorna un 'I0 ()'. Toma 'Int' y retorna un valor en consola dependiendo de su valor.
  Toma un argumento de tipo 'Int'.
-}
modo2End :: Int -> IO ()
modo2End 0 = putStrLn "La entrada solo debe contener, V, T y -"
modo2End 1 = putStrLn "Tramposo!"
modo2End 2 = putStrLn "Ganaste!"
modo2End 3 = putStrLn "La computadora ha ganado!"

{-|
  La funcion 'modo2Init' retorna un 'I0 ()'. Toma una lista de palabras y selecciona una al azar y llama a la funcion 'modo2'.
  No toma ningun argumento.
-}
modo2Init :: IO ()
modo2Init = do
    listaPalabras <- palabras
    i <- randomRIO (0, length listaPalabras - 1)
    let target = listaPalabras !! i 
    modo2 listaPalabras target 6 [] "" [target]

{-|
  La funcion 'modo2' retorna un 'I0 ()'. Toma una lista de palabras, una palabra objetivo y la cantidad de vidas o oportunidades. 
  Verifica si aun quedan oportunidades, si quedan muestra la palabra objetivo que se ha calculado anteriormente, luego recibe la entrada
  por consola del usuario y verifica que solo contenga "T","V" o "-" y su tamano sea 5. Si lo es se convierte en una lista de 'Opciones'
  y si se compone de Toros unicamente, se acaba el juego y gana la computadora. Si no, se validan los datos con la funcion 'validarE'.
  Una vez se acaben las oportunidades o se introduzca "TTTTT" se acaba el juego. Si se introduce un valor invalido, se muestra un error
  en consola y se llama de nuevo a esta funcion. 
  El argumento palabrasGeneradas aloja las palabras que ha generado la computadora al intentar adivinar, para evitar repeticiones.
  Toma seis argumentos de tipo '[String]', 'String', 'Int', '[Opciones]', 'String' y '[String]'.
-}
modo2 :: [String] -> String -> Int -> [Opciones] -> String -> [String] -> IO ()
modo2 listaPalabras target vidas guessAnterior palabraAnterior palabrasGeneradas
    | vidas == 0 = modo2End 2
    | otherwise = do
        putStr ("DESCIFRADOR  : " ++ target ++ "\n")
        hFlush stdout
        putStr "MENTE MAESTRA  : "
        hFlush stdout
        input <- getLine
        if (all (\x -> x `elem` ['T','V','-']) input) == False || length input /= 5
            then do 
                putStrLn "Entrada invalida : La entrada solo debe contener, V, T y -"
                modo2 listaPalabras target vidas guessAnterior palabraAnterior palabrasGeneradas
        else do
            let guess = Prelude.map stringAOpciones input
            if guess == [Toro,Toro,Toro,Toro,Toro]
                then modo2End 3
            else do
                validarE guess listaPalabras target vidas guessAnterior palabraAnterior palabrasGeneradas
        
{-|
  La funcion 'validarE' retorna un 'I0 ()'. Toma una lista de 'Opciones' y verifica su tamano. Si es de tamano 5, se crea un arbol
  y se busca la palabra de menor valor con la funcion 'palabraEncontrada', si el resultado es no vacio, entonces existe la palabra
  y se llama de nuevo a 'modo2' con esta palabra, si es vacia no existen mas opciones para la lista de 'Opciones' dadas y el jugador
  ha hecho trampa, ya que la palabra no existe en la lista de palabras o ha cambiado la posicion de algun Toro o Vaca.
  La lista de 'String' de palabrasGeneradas se utiliza para evitar repeticiones al encontrar una nueva palabra y se actualiza al llamar
  de nuevo a la funcion 'modo2'.
  Toma seis argumentos de tipo '[Opciones]', '[String]', 'String', 'Int', '[Opciones]' y 'String'.
-}
validarE :: [Opciones] -> [String] -> String -> Int -> [Opciones] -> String -> [String] -> IO ()
validarE guess listaPalabras palabra vidas guessAnterior palabraAnterior palabrasGeneradas
    | length guess /= 5 = do
        putStrLn "Entrada invalida"
        modo2 listaPalabras palabra vidas guessAnterior palabraAnterior palabrasGeneradas
    | otherwise = do
        let arbol = crearArbol guess palabra listaPalabras
        let tupla = palabraEncontrada arbol palabrasGeneradas
        if length tupla == 0 || (checkGuess guess guessAnterior palabra palabraAnterior == True)
           then modo2End 1
        else do
           modo2 listaPalabras (fst (tupla !! 0)) (vidas-1) guess palabra (palabrasGeneradas++[(fst (tupla !! 0))])
{-|
  La funcion 'buscarToros' retorna un 'Char'. Toma un tipo 'Opciones' y retorna T si es Toro, V si es Vaca y - si es Vacio.
  Toma un argumento de tipo 'Opciones'.
-}
buscarToros :: Opciones -> Char
buscarToros op
              | op == Toro = 'T'
              | op == Vaca = 'V'
              | otherwise = '-'
{-|
  La funcion 'checkGuess' retorna un 'Bool'. Toma dos listas de 'Opciones' y dos 'String'. Verifica si la palabra anterior es vacia,
  entonces no se encuentra ninguna alojada porque es la primera ronda dl juego. Si no es vacia, se buscan los toros en la lista de 
  'Opciones' y se crea una lista de tuplas con zip que empareja a la letra con su valor en el tipo 'Opciones'. Como se guardan como 'Char'
  se hace uso de la funcion 'compareChars' para verificar si es cierto o falso el valor del usuario, ya que se conoce la entrada anterior.
  Esta funcion previene que el jugador haga trampa y solo retorna 'True'. Cuando el jugador introduzca una combinacion de T,V,- que tenga
  sentido con las que ha colocado anteriormente.
  Toma cuatro argumentos de tipo '[Opciones]','[Opciones]','String' y 'String'.
-}
checkGuess :: [Opciones] -> [Opciones] -> String -> String -> Bool
checkGuess actual anterior palabra palabraAnterior = 
                         if anterior == []
                             then False
                         else do
                             let act = zip (map buscarToros actual) palabra
                             let ant = zip (map buscarToros anterior) palabraAnterior
                             if (compareChars act ant) == True
                                 then True
                             else 
                                 False
    where
        {-|
           La funcion 'compareChars' retorna un 'Bool'. Toma dos listas de '(Char,Char)' y analiza cada uno de sus elementos
           con la funcion 'comparacionC' y luego consigue el valor de todos con la funcion 'and'.
           Toma dos argumentos de tipo '[(Char,Char)]'.
        -}
        compareChars :: [(Char,Char)] -> [(Char,Char)] -> Bool
        compareChars actual anterior = and (zipWith comparacionC actual anterior)
        {-|
           La funcion 'comparacionC' retorna un 'Bool'. Toma '(Char,Char)' y analiza los primeros y segundos elementos de cada tupla.
           Los primeros elementos son 'V','T' o '-' dependiendo de lo que introduce el usuario. Y los segundos elementos de la tupla
           son las letras, si son diferentes y los primeros elementos son iguales, el jugador trata de hacer trampa. Ademas se verifican
           el resto de los casos. Retorna 'True' si el jugador intenta hacer trampa y 'False' si no.
           Toma dos argumentos de tipo '(Char,Char)'.
        -}
        comparacionC :: (Char,Char) -> (Char,Char) -> Bool
        comparacionC tupla1 tupla2
                            | (fst tupla1 == fst tupla2) && (fst tupla1 == '-') && (snd tupla1 /= snd tupla2) = False
                            | (fst tupla1 == fst tupla2) && (fst tupla1 == 'V' || fst tupla1 == 'T') && (snd tupla1 == snd tupla2) = False
                            | (fst tupla1 == fst tupla2) && (fst tupla1 == 'V' || fst tupla1 == 'T') && (snd tupla1 /= snd tupla2) = True
                            | (fst tupla1 /= fst tupla2) && (snd tupla1 /= snd tupla2) = False
                            | (fst tupla1 /= fst tupla2) && (snd tupla1 == snd tupla2) = True
                            | otherwise = False                             
{-|
  La funcion 'main' retorna un 'I0 ()'. Es la funcion principal que reciba las entradas por consola y llama a cada uno de los modos
  de juego. Si no se introduce un argumento o si el argumento es invalido, imprime un error. Sino, llama a la funcion 'modo1Init' o 
  'modo2Init' dependiendo del argumento dado por consola. 
  No toma ningun argumento.
-}
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
