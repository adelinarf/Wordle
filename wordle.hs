import Data.Map ( fromListWith, Map )
import qualified Data.Map as Map
import System.Directory.Internal.Prelude
    ( hFlush, stdout, getArgs )
import System.IO ()
import System.Environment (getArgs)
import System.Random

import Data.Char (toUpper)


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


data Arbol a  b c =  Empty  |  Nodo a b c [ Arbol a b c] deriving Show
--a es el nombre de la palabra, b es la posibilidad y c es el puntaje
crearArbol :: [String] -> [Arbol String String Int]
crearArbol = Prelude.map (\ x -> Nodo x "posibilidad" 0 [])


-- crear :: [String] -> [Arbol String String Int]
-- crear = scanr arbolI "pos"
--     where
--         arbolI :: String -> String -> Arbol String String Int
--         arbolI x = Nodo x "posibilidad" 0 []

--readFile "nombre.txt" con esto se lee el archivo se crea una estructura que aloje las palabras y las divida por \n
--endBy ";" "foo;bar;baz;"
--["foo","bar","baz"]

palabras :: IO [String]
palabras = do
    t <- readFile "Palabras.txt"
    let palabras = lines t
    return palabras


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
        validate target log lives (map (convertV . toUpper) guess) p

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


modo2Init :: IO ()
modo2Init = putStrLn "modo 2"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "es necesario el argumento de modo de juego"
        [x]-> case  head args of
            "mentemaestra" -> modo1Init
            "descifrador" -> modo2Init
            _ -> putStrLn "argumento invalido"
        _-> putStrLn $ "comando inválido: " ++ args !! 1
