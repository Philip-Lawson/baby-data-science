module Main where

import BabyDataScience
import qualified Data.Text.IO as T.IO
import qualified Data.Text as T
import qualified System.IO.Strict as S
import qualified Data.Char as C
import System.Environment
import System.Directory

data Command = Learn FilePath
             | Tweak FilePath
             | Stats 
             | Error 

trainingState = "BabyDataScienceTrainingState"
tempFile = trainingState ++ ".tmp"

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
      Tweak file -> tweak file
      Stats -> printStats trainingState
      Learn file -> goLearn file
      _ -> putStrLn "learn <filename> | tweak <filename> | stats"

parseArgs :: [String] -> Command
parseArgs ("learn":fileName:_) = Learn fileName
parseArgs ("tweak":fileName:_) = Tweak fileName
parseArgs ("stats":_) = Stats
parseArgs _ = Error

goLearn :: String -> IO ()
goLearn file = do
    trainingData <- T.IO.readFile file
    let result = learn emptyResultSet trainingData
    writeFile trainingState (show result)

tweak :: String -> IO ()
tweak file = do
    rawResultSet <- S.readFile trainingState
    reviews <- T.IO.readFile file
    let resultSet = read rawResultSet :: ResultSet
    let newResults = getResults resultSet reviews
    tweakedResults <- tweakResults newResults
    let newResultSet = learn resultSet tweakedResults
    writeFile tempFile (show newResultSet)
    renameFile tempFile trainingState

tweakResults :: [(Int, T.Text)] -> IO T.Text
tweakResults newResults = do
    realData <- mapM getCorrectResult newResults
    return (T.unlines realData)

getCorrectResult :: (Int, T.Text) -> IO T.Text
getCorrectResult (rating, entry) = do
    putStrLn ("Rating: " ++ show rating)
    putStrLn "Entry:"
    putStrLn (T.unpack entry)
    putStrLn "Is the rating correct? [y|n]"
    answer <- readLn :: IO Char
    case answer of
      'y' -> return entry
      _ -> do
        putStrLn "Enter Correct Rating: "
        n <- readLn :: IO Int
        return (T.cons (C.intToDigit n) entry)

printStats :: String -> IO ()
printStats file = do
    rawResultSet <- S.readFile file
    let resultSet = read rawResultSet :: ResultSet
    mapM_ putStrLn (getStats resultSet)
