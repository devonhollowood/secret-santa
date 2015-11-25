import Control.Monad.Random

main = do
    people <- readPeopleFile
    forbidden_pairs <- readForbiddenPairs
    santas <- evalRandIO $ selectSantas people forbidden_pairs
    sendEmails santas

readPeopleFile :: IO [Person]
readPeopleFile = undefined

readForbiddenPairs :: IO Santas
readForbiddenPairs = undefined

selectSantas :: RandomGen g => [Person] -> Santas -> Rand g Santas
selectSantas = undefined

sendEmails :: Santas -> IO ()
sendEmails = undefined

type Santas = [(Person, Person)]

data Person = Person {
    name :: String,
    email :: String
}
