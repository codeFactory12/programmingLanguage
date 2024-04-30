module HouseSort where

import System.IO
import System.Random
import Control.Monad (liftM)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

type StudentName = String

data Student = Student {
    lastName :: String,
    name     :: StudentName
} deriving (Show, Eq, Ord)

data SchoolHouse = Griffindor
                 | Hufflepuff
                 | Ravenclaw
                 | Slytherin
                 deriving (Eq, Show, Ord, Read)

assignSchoolHouse :: Int -> [(SchoolHouse, Int)] -> IO (Maybe SchoolHouse)
assignSchoolHouse _ [] = return Nothing
assignSchoolHouse maxStudents ((house, count):houses)
    | count < maxStudents = return (Just house)
    | otherwise           = assignSchoolHouse maxStudents houses

pickRandom :: [a] -> IO a
pickRandom xs = do
    idx <- randomRIO (0, length xs - 1)
    return (xs !! idx)

split' :: Eq a => a -> [a] -> [[a]]
split' _ [] = []
split' d s = x : split' d (drop 1 y)
    where (x,y) = span (/= d) s

parseLine :: String -> Student
parseLine line =
    let [lastName, name] = split' ',' line
    in Student { lastName = lastName, name = name }

readFileToList :: FilePath -> IO [Student]
readFileToList filePath = do
    contents <- readFile filePath
    let students = map parseLine (lines contents)
    return students

main :: IO ()
main = do
    records <- readFileToList "/Users/azamorano/Documents/usb/programmingLanguages/2024/list.txt"
    let maxStudentsPerHouse = 5  -- Set your desired max students per house here
    houseCounts <- initializeHouseCounts maxStudentsPerHouse
    let studentsList [] _ = return ()
        studentsList (x:xs) houses = do
            maybeHouse <- assignSchoolHouse maxStudentsPerHouse houses
            case maybeHouse of
                Just schoolHouse -> do
                    let newHouses = updateHouseCount schoolHouse houses
                    putStrLn $ show (x, schoolHouse)
                    studentsList xs newHouses
                Nothing -> putStrLn "No more available slots for any house."
    studentsList records houseCounts

initializeHouseCounts :: Int -> IO [(SchoolHouse, Int)]
initializeHouseCounts maxStudentsPerHouse = return [(Griffindor, 0), (Hufflepuff, 0), (Ravenclaw, 0), (Slytherin, 0)]

updateHouseCount :: SchoolHouse -> [(SchoolHouse, Int)] -> [(SchoolHouse, Int)]
updateHouseCount house counts =
    let update (h, count) = if h == house then (h, count + 1) else (h, count)
    in map update counts
