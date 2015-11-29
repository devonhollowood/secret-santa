import qualified Network.HaskellNet.SMTP as SMTP
import Options.Applicative as Opt
import Control.Monad.Random
import Control.Monad (mzero, liftM, when)
import Data.Csv hiding (Name)
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V
import qualified Data.Text.Lazy as T
import System.Random.Shuffle
import System.Directory (doesFileExist)
import System.Exit (die)
import Data.List (find)
import Data.Maybe (maybeToList)

main = do
    opts <- execParser parseOpts
    people <- readPeopleFile (peopleFile opts)
    forbidden_pairs <- maybe
                      (return [])
                      readForbiddenPairsFile
                      (forbiddenPairsFile opts)
    maybe_santas <- evalRandIO $ selectSantas people forbidden_pairs
    case maybe_santas of
        Just santas -> santaAction opts santas
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
    liftM                            -- Do in the Rand g Monad
    (find $ not . any is_forbidden)  -- Pick first list with no forbidden pairs
    (generatePairs people)          -- From an infinite list of lists of pairs
    where is_forbidden :: (Person, Person) -> Bool
          is_forbidden (a, b) =
              name a == name b ||
              (name a, name b) `elem` forbidden ||
              (name b, name a) `elem` forbidden

{- Generates an infinite list of lists of randomly-paired Persons from `people`
 -}
generatePairs :: RandomGen g => [Person] -> Rand g [[(Person, Person)]]
generatePairs people =
    liftM                            -- Do in the Rand g Monad
    (map $ zip people)               -- Pair up shuffled lists of people
    (mapM shuffleM $ repeat people)  -- Create lists of shuffled people forever

{- Action which prints out the assignments, and nothing else
 -}
testSantas :: Santas -> IO ()
testSantas = mapM_ (putStrLn . santaLine)
    where santaLine (a, b) =
              concat ["Assigned ", name a, " (", email a,
                      ") their secret santa: ", name b]

{- Action which sends out emails to the Santas in `santas`
 -}
sendEmails :: Santas -> IO ()
sendEmails santas = do
    (username, connection) <- connect -- Connect to server
    doubleSendGuard -- Make sure we haven't already sent the emails
    putStrLn "Sending emails..."
    mapM_ (sendEmail connection username) santas -- Send the emails
    putStrLn "\t...success!"
    SMTP.closeSMTP connection -- Close connection
    writeFile doubleSendGuardFile "" -- Protect against future double-sends

{- Sends a secret santa email to `gifter`, assigning them `giftee`
 - `connection` is the SMTP connection to use
 - `username` is the username to log in with
 -}
sendEmail :: SMTP.SMTPConnection -> UserName -> (Person, Person) -> IO ()
sendEmail connection username (gifter, giftee) =
    SMTP.sendPlainTextMail     -- Send email
    (email gifter)             -- Recipient
    (username ++ "@gmail.com") -- Sender
    "Secret Santa (Shhh!)"     -- Subject
    (T.pack $ unlines          -- Body
        ["Hello" ++ name gifter ++ ",",
         "Your secret santa giftee is " ++ name giftee,
         "Get them something good!"
        ]
    )
    connection                 -- SMTP Connection

{- Connects to gmail, prompts for username and password, and returns
 - the username (e.g. "devonhollowood") for the connection, as well as the
 - connection itself
 -}
connect :: IO (UserName, SMTP.SMTPConnection)
connect = do
    username <- prompt "What is your gmail username?"
    password <- prompt "What is your gmail password?"
    putStrLn "Connecting..."
    connection <- SMTP.connectSMTPPort "smtp.gmail.com" 587
    putStrLn "\t...success!"
    putStrLn "Authenticating..."
    successful <- SMTP.authenticate SMTP.PLAIN username password connection
    if successful
        then putStrLn "\t...success!" >> return (username, connection)
        else die "Authentication failed"
    where prompt msg = putStrLn msg >> getLine

{- Guards against accidentally sending the secret santa information twice
 - by checking to see if the file `doubleSendGuardFile` already exists
 -}
doubleSendGuard :: IO ()
doubleSendGuard = do
    already_sent <- doesFileExist doubleSendGuardFile
    when already_sent $ die "Secret Santa emails already sent!"

{- Filename for the already-sent file. Used to protect against accidentally
 - sending out the emails twice
 -}
doubleSendGuardFile :: FileName
doubleSendGuardFile = "already-sent.guard"

{- Type declarations
 -}
type FileName = String
type Name = String
type Email = String
type UserName = String

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

{- Boring command line parsing stuff below this point! -}

{- Datatype representing command-line options
 -}
data Options = Options {
    santaAction :: Santas -> IO (),
    peopleFile :: FileName,
    forbiddenPairsFile :: Maybe FileName
}

{- High-level program description
 -}
parseOpts :: ParserInfo Options
parseOpts = info (helper <*> options)
            (  fullDesc
            <> progDesc "generate and assign secret santa pairings"
            <> Opt.header "secret-santa: a tool to run secret santa pairings"
            )

{- Program option parsing
 -}
options :: Opt.Parser Options
options =
    Options
    <$> subparser (
            command "test" (
                info (pure testSantas) (progDesc
                "assign secret santa pairings, but do not send emails")
            )
         <> command "execute" (
                info (pure sendEmails) (progDesc
                "assign secret santa pairings and email assignments")
            )
         <> help "choose command to execute"
         <> metavar "<command>"
        )
    <*> strArgument (
            metavar "<people file>"
         <> help "CSV file containing two columns: [Name, Email]"
        )
    <*> optional (
            strOption (
            long "forbidden-pairs"
            <> short 'x'
            <> metavar "<forbidden pairs file>"
            <> help "CSV file containing forbidden pairs in format \
                    \[Name, Name]"
            )
        )
