module BabyDataScience
    ( ResultSet, learn, emptyResultSet, getStats, getResults
    ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Char
import Data.List
import Data.Ord

type Rating = Double
type Count = Double
type Sum = Double
type Words = [T.Text]
type NewEntry = (Rating, Count)
type MovingAverage = (Rating, Count)
type ResultSet = M.Map T.Text MovingAverage

emptyResultSet :: ResultSet
emptyResultSet = M.empty

learn :: ResultSet -> T.Text -> ResultSet 
learn resultSet = foldr processLine resultSet . parseValidText 

getStats :: ResultSet -> [String]
getStats resultSet = ["Best word: " ++ topWord
                    , "Worst word: " ++ worstWord 
                    , "Words processed: " ++ numWords] 
    where topWord = T.unpack  $ fst (last sortedResultSet)
          worstWord = T.unpack $ fst (head sortedResultSet)
          numWords = show (M.size resultSet)
          sortedResultSet = sortBy (comparing (fst . snd)) (M.toList resultSet)

getResults :: ResultSet -> T.Text -> [(Int, T.Text)]
getResults resultSet = map (getResult resultSet) . T.lines

getResult :: ResultSet -> T.Text -> (Int, T.Text)
getResult resultSet entry = (rating, entry)
    where rating = round $ getRating resultSet $ normalise entry

getRating :: ResultSet -> Words -> Rating
getRating resultSet words = rating / total 
    where (rating, total) = foldr (getTotals resultSet) (0.0, 0.0) words

getTotals :: ResultSet -> T.Text -> (Sum, Count) -> (Sum, Count) 
getTotals resultSet word (total, count) =
    case M.lookup word resultSet of
      Nothing -> (total, count)
      Just (n, _) ->  (total + n, succ count)

parseValidText :: T.Text -> [(Rating, Words)]
parseValidText = map toStructure . filter hasRating . T.lines
  where hasRating = isDigit . T.head
        rating = read . T.unpack . T.take 1
        toStructure x = (rating x, normalise (T.drop 1 x)) 

normalise :: T.Text -> Words
normalise = T.words . T.toCaseFold

processLine :: (Rating, Words) -> ResultSet -> ResultSet
processLine (rating, wordList) resultSet = foldr (processWord rating) resultSet wordList

processWord :: Rating -> T.Text -> ResultSet -> ResultSet 
processWord rating word = M.insertWith calculateMovingAverage word (rating, 1.0)

calculateMovingAverage :: NewEntry -> MovingAverage -> MovingAverage
calculateMovingAverage (a, b) (x, y)  = ((x * y / y') + (a / y'), y')
  where y' = b + y
