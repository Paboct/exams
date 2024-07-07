import Data.Bifoldable (bifoldl1)
import Data.ByteString.Builder (FloatFormat)
import Test.HUnit
import Data.Vector.Fusion.Stream.Monadic (Stream)
{-
1
    a)
        Bool
        Float
        [Int]
        (String, Bool)
    
    b)
        phi :: Float
        null' :: [a] -> Bool
        slope :: Num a => (a,a) -> (a,a) -> a
        isContradiction :: Bool -> Bool -> Bool
    
    c)
        No
        No
        No
        Si
        No
        Si
        Si
        Si
        No
        Si

2
    1
    16
    True
    [11,9,7,5,3,1]
    False
    [2,3,4]
    (1,2)
    "c" ó ['c']

3
    sumSqr :: Int -> Int
    sumSqr x = sum [x * x | x <- [1..n]]

4
    tensDigit :: Integral a => a -> a
    tensDigit x = d
        where xLast = x `div` 10
              d = xLast `mod` 10

-}

--5 a
isVowel :: Char -> Bool
isVowel c = if elem c "aeiouAEIOU"
            then True
            else False

--b
imc :: Float -> Float -> String
imc w h | delta < 18.5 = "Peso bajo"
        | delta < 24.9 = "Normal"
        | delta < 29.9 = "Sobrepeso"
        | otherwise = "Obesidad"
        where delta = w / (h**2)

hasTwoElements :: [a] -> Bool
hasTwoElements [_,_] = True
hasTwoElements (_:xs) = False
hasTwoElements [] = False


--6
--cylinderArea :: Num a => a -> a -> a
--cylinderArea = \r -> \h -> 2 * pi * r * h

{-distance :: Floating a => (a,a,a) -> [a] -> a
distance = \(x,y,z) ->  \[a,b,c,k] -> num / denom
    where num = abs (a*x + b*y + c*z + k)
          denom = sqrt (a*a + b*b + c*c)
-}

function :: Int -> [Int]
function n = if n >= 0
            then [i | i <- [n, n+3, n+6, n+9]]
            else  [i | i <- [n, n-3, n-6, n-9]]

concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

testConcat :: Test
testConcat = test[
    "Empty List" ~: concat' ([]::([[Int]])) ~?= ([]::[Int]),
    "One Elt" ~: concat' [[1]] ~?= [1],
    "Two elm" ~: concat' [[1],[2]] ~?= [1,2],
    "One and Empty" ~: concat' [[1], []] ~?= [1],
    "Empty and One" ~: concat' [[], [1]] ~?= [1]
    ]

{-
8
    a
        Crea una lista que contiene el elemento pasado
        como parametro repeteido n veces

    b
        g :: Int -> a -> [a]
        g n x = [x | _ <- [0,2..n]]
-}

--9
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

anyBornIn :: String  -> [(String, String, String, Int)] -> Bool
anyBornIn y db = or [ y == c| (_,_,c,_) <- db]

bornInDecade :: Int -> [(String, String, String, Int)] -> Int
bornInDecade d db = sum [1 | (_,_,_,n) <- db, isDecade d n == 1]
    where isDecade d n = if n >= d && n < d+10
                        then 1
                        else 0