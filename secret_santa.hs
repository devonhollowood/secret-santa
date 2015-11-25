import Control.Monad.Random
import Control.Monad (mzero)
import Data.Csv hiding (Name)
import Data.ByteString.Lazy as B
import Data.Vector as V
import System.Exit (die)

main = do
    people <- readPeopleFile "people.txt"
    forbidden_pairs <- readForbiddenPairsFile "forbidden-pairs.txt"
    santas <- evalRandIO $ selectSantas people forbidden_pairs
    sendEmails santas

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
readCsv :: FromRecord a => ByteString -> Either String [a]
readCsv csv_data = V.toList <$> decode NoHeader csv_data

selectSantas :: RandomGen g => [Person] -> [ForbiddenPair]  -> Rand g Santas
selectSantas = undefined

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
