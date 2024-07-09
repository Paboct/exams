{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Csv
import Data.List
import GHC.Generics
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL

--player_name,club,position,minutes_played,match_played,goals,assists,distance_covered
data Player = Player
    { player_name :: String
    , club :: String
    , position :: String
    , minutes_played :: Int
    , match_played :: Int
    , goals :: Int
    , assists :: Int
    , distance_covered :: Float
    }
    deriving (Generic, Show)

instance FromNamedRecord Player
instance ToNamedRecord Player

type ErrorMsg = String
type CsvData = (Header, V.Vector Player)

main :: IO ()
main = do
    csvData <- BL.readFile "key.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> doWork v

positions :: [String]
positions = ["Defender","Forward","Goalkeeper","Midfielder"]

--Número de registros contenidas en el fichero csv
doWork :: V.Vector Player -> IO ()
--doWork = print . length

--Equipo con más jugadores que han marcado gol, junto con el número de goles
--excluyendo los goles de los defensas. ("Liverpool", 10)
doWork = print . maxGoaler . goalsPerTeam . group . orderedTeams . filterNotDefense . filterGoal

filterNotDefense :: V.Vector Player -> V.Vector Player
filterNotDefense = V.filter (\p -> position p /= "Defender")

filterGoal :: V.Vector Player -> V.Vector Player
filterGoal = V.filter (\p -> goals p > 0)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort left ++ [x] ++ qsort right
    where
        left = filter (<x) xs
        right = filter (>=x) xs

orderedTeams :: V.Vector Player -> [String]
orderedTeams = qsort . V.toList . V.map club

goalsPerTeam :: [[String]] -> [(String, Int)]
--goalsPerTeam = map (\x -> (head x, length x))
goalsPerTeam xss = zip (map head xss) (map length xss)

maxGoaler :: [(String, Int)] -> (String, Int)
maxGoaler = foldl (\acc y -> if (snd y) > (snd acc) then y else acc) ("", 0)