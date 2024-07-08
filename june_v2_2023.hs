import Data.Char (toUpper)
import Graphics.Rendering.Chart.Easy (mediumspringgreen, element)
import Data.ByteString (StrictByteString)
{-
1
    Que la funcion es un dato mas y los programas se construyen evaluando expresiones
    en lugar de ejecutarlas    
    Forma de simular que una función acepta multiples argumentos
        Ej:
            mult :: Int -> Int -> Int
            mult = \x -> \y -> x * y
    Funcion que procesa, en un orden determinado, una estructura de datos
    para reducirla a un único valor
        Ej:
            foldl (+) 1 [1,2,3,4]  
    Pasar a una función un menor número de argumentos, secciones y expr lambda
    Ejemplo: 
        double = (\x -> x * 2)
-}

--2
--a
pairs :: [a] -> [b] -> [(a,b)]
--pairs xs ys = [(x,y) | x <- xs, y <- ys]
pairs xs ys = [(xs !! i,ys !! i) | i <- [0..min(length xs)(length ys)-1]]

--b
noMultiples :: Int -> [Int] -> [Int]
noMultiples n xs = [x | x <- xs, x `mod` n  /= 0]

--c
sumEvens :: (Int, Int, Int) -> Int
sumEvens (x,y,z) = sum [n | n <- [x,y,z], even n]

--d
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x,y) <- zip xs ys]

--3
--a
mcd :: Int -> Int -> Int
mcd a b |  a == b = a
           | otherwise = mcd b (a `mod` b)

--b
toUp :: String -> String
toUp "" = ""
toUp (x:xs) = toUpper x : toUp xs

--c
countVowels :: String -> Int
countVowels "" = 0
countVowels (c:cs) = if elem c "aeiouAEIOU"
                    then  1 + countVowels cs
                    else countVowels cs
--d
lowerValues :: Ord a => a -> [a] -> [a]
lowerValues _ [] = []
lowerValues x (y:ys) = if y < x
                        then y: lowerValues x ys
                        else lowerValues x ys
--4
--a
frequency :: Eq a => a -> [a] -> Int
frequency x = length . filter (==x)

--b
applySelected :: (a -> Bool) -> (a -> b) -> [a] -> [b]
applySelected p f = map f . filter p

--c
minimum :: Ord a => [a] -> a
minimum = foldl1 (\acc x -> if x < acc then x else acc)

--5
--a
pokemons :: [(Int, String, [String])]
pokemons = [(39, "Jigglypuff", ["Normal", "Hada"]),
            (8, "Wartortle", ["Agua"]),
            (701, "Hawlucha", ["Lucha", "Volador"]),
            (80, "Slowbro", ["Agua", "Psíquico"]),
            (431, "Jynx", ["Hielo", "Psíquico"]),
            (685, "Slurpuff", ["Hada"]),
            (702, "Dedenne", ["Eléctrico", "Hada"]),
            (143, "Snorlax", ["Normal"])]

--b
hairy :: [String]
hairy = [n | (_,n,ts) <- pokemons, elem "Hada" ts]

--c
get_duple :: [(Int, String, [String])] -> [(Int, String)]
get_duple = map (\(x,y,_) -> (x,y))

--d
count_types :: String -> [(Int, String, [String])] -> Int
count_types t = length . filter (\(_,_,ts) -> elem t ts)

--e
--powerInRange :: Int -> Int -> [(Int, String, String, [String])] -> [String]
powerInRange x y = map (snd) .  get_duple . filter (\(n,_,_) -> x <= n && n <= y)