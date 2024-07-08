import Data.Char
import Test.HUnit
import Graphics.Rendering.Chart.Easy (dimgray)

{-
1
    a
    Char
    String
    [Char]ó String
    No es válido, sólo tiene une elemento

    b
    volShere :: Float -> Float
    scalarMult :: Num a => a -> (a,a) -> (a,a)
    firstAndLast :: [a] -> (a,a)
    isTautology :: Eq a => a -> a -> a -> Bool

2
    Si
    No
    Depende de la implementación
    Si
    Si
    Depende de la implementación
    Si
    Si
    Si
    Si
    No
    No
    Si

3
19
4
(1,4)
2
[(1,2,3),(4,5,6)]
[7]
[[2,1]]

3
circulo :: Int -> Int
circulo n = length [(x,y) | x <- [0..n], y <- [0..n], x*x+y*y<n*n]

4
divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], isDivisor x]
    where isDivisor y = n `mod` y == 0
-}

--5
--a
toNumber :: Char -> Int
toNumber c = if isDigit c 
            then digitToInt c 
            else error "Not a number"

--b
numRoots :: Float -> Float -> Float -> Int
numRoots a b c | delta < 0 = 0
               | delta == 0 = 1
               | otherwise = 2
               where delta = b**2 -4*a*c

--c
secureDiv :: Float -> Float -> Float
secureDiv _ 0 = error "Division by zero"
secureDiv x y = x / y

--6
--a
triangleArea :: Float -> Float -> Float
triangleArea = \b -> \h-> (b*h) / 2

--b
addPairs :: Num a => (a,a) -> (a,a) -> (a,a)
addPairs = \(x,y) -> \(z,w) -> (x+z, y+w)

--7
--a
func :: [Int] -> [Int]
func ns = [n | n <- ns, odd n && n /= 5]

--b
divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], isPerfect x]
            where isPerfect x = x == sum(init(divisors x))


testPerfects :: Test
testPerfects = test [
    "Invalid value" ~: perfects (-1) ~?= [],
    "No perfects" ~: perfects 1 ~?= [],
    "Perfects" ~: perfects 10 ~?= [6]
    ]

{-8
    a
    Da el doble de elementos de la tripla que sean pares
    b
    Cambiar el 2 por 1
-}

informaticos :: [(String,String,String,Int)]
informaticos = [("Lovelace","Ada","UK",1815),
                ("Hopper","Grace","USA",1906),
                ("Backus","John","USA",1924),
                ("Minsky","Marvin","USA",1927),
                ("McCarthy","John","USA",1927),
                ("Dijkstra","Edsger W.","Netherlands",1930),
                ("Knuth","Donald","USA",1938),
                ("Leslie","Lamport","USA",1941),
                ("Ritchie","Dennis M.","USA",1941),
                ("Kernighan","Brian W.","Canada",1942),
                ("Cerf","Vinton","USA",1943),
                ("Thompson","Ken","USA",1943),
                ("Tanenbaum","Andrew S.","USA",1944),
                ("Stallman","Richard","USA",1953),
                ("Berners-Lee","Tim","UK",1955),
                ("van Rossum","Guido","Netherlands",1956),
                ("Fowler","Martin","UK",1963),
                ("Torvalds","Linus","Finland",1969)
                ]

surnames :: [(String,String,String,Int)] -> [String]
surnames db = [s | (s,_,_,_) <- db]

bornIn :: Int -> [(String,String,String,Int)]-> [(String, String)]
bornIn y db = [(s, n) | (s,n,_,x) <- db, x == y]

--10
--Crea una copia de un repositorio existente en un nuevo directorio