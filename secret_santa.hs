import Control.Monad.Random
import Control.Monad (mzero)
import Data.Csv hiding (Name)
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V
import System.Random.Shuffle
import System.Exit (die)
import Data.List (find)

main = do
    people <- readPeopleFile "people.txt"
    forbidden_pairs <- readForbiddenPairsFile "forbidden-pairs.txt"
    maybe_santas <- evalRandIO $ selectSantas people forbidden_pairs
    case maybe_santas of
        Just santas -> sendEmails santas
        Nothing -> die "Error: no valid pairings are possible!"

{- Reads list of people from csv file `filename`.
 - Columns in `filename` should be [Name, Email]
 -}
readPeopleFile :: FileName -> IO [Person]
readPeopleFile filename = do
    csv_data <- B.readFile filename
    case readCsv csv_data of
        Right people -> return people
        Left err -> die err

{- Reads list of forbidden pairs from csv file `filename`.
 - Columns in `filename` should be [Name1, Name2]
 -}
readForbiddenPairsFile :: FileName -> IO [ForbiddenPair]
readForbiddenPairsFile filename = do
    csv_data <- B.readFile filename
    case readCsv csv_data of
        Right pairs -> return pairs
        Left err -> die err

{- Takes raw csv file data, and returns either an error message or a list of
 - `a`s contained in the file data
 -}
readCsv :: FromRecord a => B.ByteString -> Either String [a]
readCsv csv_data = V.toList <$> decode NoHeader csv_data

{- Randomly pairs people with a secret santa, rejecting any sets which contain
 - forbidden pairs
 -}
selectSantas ::
    RandomGen g => [Person] -> [ForbiddenPair] -> Rand g (Maybe Santas)
selectSantas people forbidden =
    enumeratePairs people >>= return . find (not . any is_forbidden)
    where is_forbidden :: (Person, Person) -> Bool
          is_forbidden (a, b) =
              name a == name b ||
              (name a, name b) `elem` forbidden ||
              (name b, name a) `elem` forbidden

enumeratePairs :: RandomGen g => [Person] -> Rand g [[(Person, Person)]]
enumeratePairs people =
    mapM shuffleM (repeat people) >>= return . map (zip people)

sendEmails :: Santas -> IO ()
sendEmails = undefined

type FileName = String
type Name = String
type Email = String

type Santas = [(Person, Person)]
type ForbiddenPair = (Name, Name)

data Person = Person {
    name :: Name,
    email :: Email
} deriving Show

instance FromRecord Person where
    parseRecord record
        | V.length record == 2 = Person <$> record .! 0 <*> record .! 1
        | otherwise = mzero
