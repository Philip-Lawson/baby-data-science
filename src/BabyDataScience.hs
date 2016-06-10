module BabyDataScience
    ( ResultSet, learn, emptyResultSet, getStats, getResults
    ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Char
import Data.Maybe
import Data.List
import Data.Ord

type Rating = Double
type Count = Double
type Sum = Double
type Words = [T.Text]
type NewEntry = (Rating, Count)
type MovingAverage = (Rating, Count)
type ResultSet = M.Map T.Text MovingAverage

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
getRating resultSet = uncurry (/) . foldr (calculateRating resultSet) (0.0, 0.0) 

calculateRating :: ResultSet -> T.Text -> (Sum, Count) -> (Sum, Count) 
calculateRating resultSet word (sum, count) =
    case M.lookup word resultSet of
      Nothing -> (sum, count)
      Just (n, _) ->  (sum + n, succ count)

parseValidText :: T.Text -> [(Rating, Words)]
parseValidText = map toStructure . filter hasRating . T.lines
  where hasRating = isDigit . T.head
        getRating = read . T.unpack . T.take 1
        toStructure x = (getRating x, normalise (T.drop 1 x)) 

normalise :: T.Text -> Words
normalise = T.words . T.toCaseFold

processLine :: (Rating, Words) -> ResultSet -> ResultSet
processLine (rating, words) resultSet = foldr (processWord rating) resultSet words

processWord :: Rating -> T.Text -> ResultSet -> ResultSet 
processWord rating word = M.insertWith calculateMovingAverage word (rating, 1.0)

calculateMovingAverage :: NewEntry -> MovingAverage -> MovingAverage
calculateMovingAverage (a, b) (x, y)  = ((x * y / y') + (a / y'), y')
  where y' = b + y
