import Data.Type.Equality (apply)
import GHC.Float (leDouble)
import Data.Csv.Incremental (Parser(Fail))
{-
    Ejercicio 1
    Que las expresiones se evaluan cuando sean necesarias
    Una lista esta forma por eltos heterogeneos y una tupla por eltos homogeneos
    La recursividad de cola, se basa en que lo último que se hace es la llamada recursiva
        Ej:
            factorial n = go n 1
                where
                    go 0 acc = acc
                    go n acc = go (n-1) (n*acc)

    Una función de orden superior es una funcion que recibe como argumento otra funcion
    y devuelve otra funcion como resultado
        Ej:
            map (+) [1..10]
-}

--Ejercicio 2
--a
deleteElts :: (a -> Bool) -> [a] -> [a]
deleteElts p xs = [x | x <- xs, p x]

--b
applyAll :: (a -> b) -> [a] -> [b]
applyAll f xs = [f x | x <- xs]

--c
countWhite :: String -> Int
countWhite ss = length [c | c <- ss, c == ' ']

numbersInRange :: Int -> Int -> [Int] -> [Int]
numbersInRange x y ns = [n | n <- ns, n >= x && n <= y]

--Ejercicio 3
--a
squareSum :: Int -> Int
squareSum n | n <= 0 = 0
            | otherwise = n^2 + squareSum (n-1)

--b
concat_List:: [[a]] -> [a]
concat_List [] = []
concat_List (xs:xss) = xs ++ concat_List xss

--c
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = a: replicate' (n-1) a

inList :: Eq a => a -> [a] -> Bool
inList _ [] = False
inList y (x:xs) = x == y || inList y xs

--Ejercicio 4
--a
negativeSelection :: [Int] -> [Int]
negativeSelection = filter (\x -> x < 0)

--b
digitInt :: [Int] -> Int
digitInt = foldl (\acc x -> acc*10 + x) 0

--c
--listMax :: Ord a => [a] -> [a]
--listMax = foldl (\x y -> if x >= y then x else y) 0

--d
norm :: (Num a, Floating a) => [a] -> a
norm = sqrt . sum . map (^2) 


--Ejercicio 5
--a
neighbours :: [(Int, String, [String])]
neighbours = [(165, "Filiberto", ["Azul", "Blanco"]),
            (294, "Dulce", ["Marrón", "Verde"]),
            (264, "Munchi", ["Blanco", "Azul"]),
            (275, "Bombo", ["Morado", "Azul"]),
            (431, "Narciso", ["Negro", "Gris"]),
            (129, "Pinta", ["Rosa"]),
            (176, "Rizolda", ["Blanco", "Azul"]),
            (287, "Lilu", ["Morado", "Blanco"])]

--b
white :: [String]
white = [n | (_,n,cs) <- neighbours ,elem "Blanco" cs]

--c
get_duple :: [(Int, String, [String])] -> [(Int, String)]
get_duple = map (\(n,ss,_) -> (n,ss))

--d
likesColour :: String -> [(Int, String, [String])] -> Int
likesColour ss =  length .filter(\(_,_,cs) -> elem ss cs)

--e
numberInRange :: Int -> Int -> [(Int, String, [String])] -> [String]
numberInRange x y = map (snd) . get_duple . filter(\(n,_,_) -> if n >= x && n<= y then True else False)