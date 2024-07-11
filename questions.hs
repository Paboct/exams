{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Csv
import GHC.Generics
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Char
import Data.Maybe
import Foreign.C (throwErrnoPathIfMinus1)
import Graphics.Rendering.Chart.Easy (goldenrod, Plated)
import Control.Arrow (ArrowChoice(left, right))
import Data.Version (Version(Version))
import GHC.Conc (orElse)

-- date,home_team,away_team,team,scorer,minute,own_goal,penalty

data Scorer = Scorer 
    { date :: String,
    home_team :: String,
    away_team :: String,
    team :: String,
    scorer :: String,
    minute :: String,
    own_goal :: String,
    penalty :: String
    } deriving (Generic, Show)

instance FromNamedRecord Scorer
instance ToNamedRecord Scorer

main :: IO ()
main = do
    csvData <- BL.readFile "dataBasa2.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> doWork v

doWork :: V.Vector Scorer -> IO ()

--Número de registros
--doWork = print . V.length

--Número de penalties
--doWork =  print . V.length . filterPenalty

filterPenalty :: V.Vector Scorer -> V.Vector Scorer
filterPenalty = V.filter (\p -> penalty p == "TRUE")

--Número degoles de Telmo Zarra
--doWork = print . V.length . filterScorer "Telmo Zarra"

filterScorer :: String -> V.Vector Scorer -> V.Vector Scorer
filterScorer ss = V.filter (\p -> scorer p == ss)

--País que más goles ha marcado y cuantos--('Brazil', 1046)
--doWork = print . maxGoaler . goalsPerTeam . group . orderedTeams

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort left ++ [x] ++ qsort right
    where
        left = filter (<= x) xs
        right = filter (>x) xs

teams :: V.Vector Scorer -> [String]
teams = V.toList . V.map team

orderedTeams :: V.Vector Scorer -> [String]
orderedTeams = qsort . teams

goalsPerTeam :: [[String]] -> [(String, Int)]
--goalsPerTeam = map (\xs -> (head xs, length xs))
goalsPerTeam xs = zip (map head xs) (map length xs)

maxGoaler :: [(String, Int)] -> (String, Int)
maxGoaler = foldl (\acc x -> if snd x > snd acc then x else acc) ("", 0)

--Goles de Venezuela en minuto par
--doWork = print . V.length . filterEvenMinute . filterCountry "Venezuela"

filterCountry :: String -> V.Vector Scorer -> V.Vector Scorer
filterCountry ss = V.filter (\p -> team p == ss || away_team p == ss)

filterEvenMinute :: V.Vector Scorer -> V.Vector Scorer
filterEvenMinute = V.filter (\p -> even(digitToInt(last(minute p))))

--Que jugador ha marcado más goles en propia en primera parte y cuantos han sido
--("Cristian Brolli", 3)
--doWork = print . maxGoaler . goalsPerPlayer . group . orderedScorers . filterOwnDoor

filterOwnDoor :: V.Vector Scorer -> V.Vector Scorer
filterOwnDoor = V.filter (\p -> own_goal p == "TRUE")

scorers :: V.Vector Scorer -> [String]
scorers = V.toList . V.map scorer

orderedScorers :: V.Vector Scorer -> [String]
orderedScorers = qsort . scorers

goalsPerPlayer :: [[String]] -> [(String, Int)]
goalsPerPlayer xss= zip (map head xss) (map length xss)

--Jugador con más goles fuera de casa y cuantos han sido (Cristiano Ronaldo, 49)
--doWork = print . maxGoaler . goalsPerPlayer . group . orderedScorers . filterVisitor

filterVisitor :: V.Vector Scorer -> V.Vector Scorer
filterVisitor = V.filter (\p -> team p == away_team p)

--Número de goles que ha marcado "Messi"-- ("Lionel Messi", 54)
--doWork = print . V.length . filterScorer "Lionel Messi"

--Judaor qhe ha marcado más goles justo en el minuto 5 y cuantos han sido ("Viorel Moldovan", 3)
--doWork = print . maxGoaler . goalsPerPlayer . group . orderedScorers . filterMinute

filterMinute :: V.Vector Scorer -> V.Vector Scorer
filterMinute = V.filter (\p -> minute p == "5")

--País que ha metido más goles entre 1958 y 2000-- ("Germany", 438)
--doWork = print . maxGoaler . goalsPerTeam . group . orderedTeams . filterYears "1958" "200"

filterYears :: String -> String -> V.Vector Scorer -> V.Vector Scorer
filterYears x y = V.filter (\p -> take 4 (date p) >= x && take 4 (date p) < y)

--Funcion que devuelve el promedio de minutos en los que se marcaron goles, exclyendo los goles en propia puerta
doWork = print .goalsPerMInute . exclueNA . group . orderedMinutes . exclueOwnGoals

exclueOwnGoals :: V.Vector Scorer -> V.Vector Scorer
exclueOwnGoals = V.filter (\p -> own_goal p == "FALSE")

minutes :: V.Vector Scorer -> [String]
minutes = V.toList . V.map minute

orderedMinutes :: V.Vector Scorer -> [String]
orderedMinutes = qsort . minutes

exclueNA :: [[String]] -> [[String]]
exclueNA = filter (\xs -> head xs /= "NA")

goalsPerMInute :: [[String]] -> [(String, Int)]
goalsPerMInute = map (\xs -> (head xs, length xs))
--goalsPerMInute xss = zip (map head xss) (map length xss) 