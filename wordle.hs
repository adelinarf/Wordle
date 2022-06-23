import Data.Map
import qualified Data.Map as Map

data Opciones = Toro | Vaca | Vacio | Problema deriving Show

g :: String -> String -> ([Either Opciones Char],Map Char Int)
g target guess = (zipWith toroVaca target guess, Prelude.foldr (crearDict) (fromListWith (+) [(c, 1) | c <- target]) (zip target guess))
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
h lista diccionario = fst (Prelude.foldr (buscar) ([],diccionario) lista)
    where
        buscar :: Either Opciones Char -> ([Opciones],Map Char Int) -> ([Opciones],Map Char Int)
        buscar (Left opcion) (ansSoFar,mem) = (agregar opcion ansSoFar,mem)
        buscar (Right palabra) (ansSoFar,mem) = 
            if Map.lookup palabra mem > Just 0
                then (agregar Vaca ansSoFar,Map.adjust pred palabra mem)
                else (agregar Vacio ansSoFar,mem)

append :: Opciones -> [Opciones] -> [Opciones]
append a xs = xs ++ [a]
agregar :: Opciones -> [Opciones] -> [Opciones]
agregar a xs = [a] ++ xs
--verificar que la letra este en el diccionario
-- que sea mayor que 0
-- si cumple retorna V

conseguir :: String -> String -> [Opciones]
conseguir target = (uncurry h) . g target


data Arbol a  b c =  Empty  |  Nodo a b c [ Arbol a b c] deriving Show
--a es el nombre de la palabra, b es la posibilidad y c es el puntaje
crearArbol :: [String] -> [Arbol String String Int]
crearArbol [] = []
crearArbol (x:xs) = [Nodo x "posibilidad" 0 []] ++ crearArbol xs  


crear :: [String] -> [Arbol String String Int]
crear lista = scanr arbolI "pos" lista
    where
        arbolI :: String -> String -> Arbol String String Int
        arbolI x = Nodo x "posibilidad" 0 []
        
--readFile "nombre.txt" con esto se lee el archivo se crea una estructura que aloje las palabras y las divida por \n
--endBy ";" "foo;bar;baz;"
--["foo","bar","baz"]
