import qualified Network.HaskellNet.SMTP.SSL as SMTP
import Options.Applicative as Opt
import Control.Monad.Random (evalRandIO, RandomGen, Rand)
import Control.Monad (mzero, when)
import qualified Data.Csv as Csv
import Data.Csv ((.!))
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V
import qualified Data.Text.Lazy as T
import qualified Data.Map.Strict as Map
import System.Random.Shuffle (shuffleM)
import System.Directory (doesFileExist)
import System.Exit (die)
import System.IO (hFlush, stdout)
import Data.List (find)

main :: IO ()
main = do
    opts <- execParser parseOpts -- Get command line options
    people <- readPeopleFile (peopleFile opts) -- Get list of people
    forbidden_pairs <- maybe                   -- Get list of forbidden pairs
                      (return [])
                      readForbiddenPairsFile
                      (forbiddenPairsFile opts)
    addresses <- maybe                         -- Get mailing address book
                 (return Map.empty)
                 readMailingAddressFile
                 (mailingAddressFile opts)
    maybe_santas <- evalRandIO $               -- Get Maybe (list of pairings)
        selectSantas people forbidden_pairs
    case maybe_santas of                       -- Perform action chosen at
        Just santas ->                         -- command line if possible
            santaAction opts addresses santas
        Nothing -> die "Error: no valid pairings found!"

{- Reads list of people from csv file `filename`.
 - Columns in `filename` should be [Name, Email]
 -}
readPeopleFile :: FileName -> IO [Person]
readPeopleFile filename = do
    csv_data <- B.readFile filename   -- Try to read list-of-people file
    case readCsv csv_data of          -- Make sure we were successful
        Right people -> return people
        Left err -> die err

{- Reads list of forbidden pairs from csv file `filename`.
 - Columns in `filename` should be [Name1, Name2]
 -}
readForbiddenPairsFile :: FileName -> IO [ForbiddenPair]
readForbiddenPairsFile filename = do
    csv_data <- B.readFile filename -- Try to read forbidden pairs file
    case readCsv csv_data of        -- Make sure we were successful
        Right pairs -> return pairs
        Left err -> die err

{- Reads dictionary of mailing addresses from csv file `filename`.
 - Columns in `filename` should be [Name, Mailing Address]
 -}
readMailingAddressFile :: FileName -> IO MailingAddressBook
readMailingAddressFile filename = do
    csv_data <- B.readFile filename -- Try to read forbidden pairs file
    case readCsv csv_data of        -- Make sure we were successful
        Right pairs -> return . Map.fromList $ pairs
        Left err -> die err

{- Takes raw csv file data, and returns either an error message or a list of
 - `a`s contained in the file data
 -}
readCsv :: Csv.FromRecord a => B.ByteString -> Either String [a]
readCsv csv_data = V.toList <$> Csv.decode Csv.NoHeader csv_data

{- Randomly pairs people with a secret santa, rejecting any sets which contain
 - forbidden pairs
 -}
selectSantas ::
    RandomGen g => [Person] -> [ForbiddenPair] -> Rand g (Maybe Santas)
selectSantas people forbidden =
    fmap                                        -- Do in the Rand g Monad
    (find (not . any is_forbidden) . take 1000) -- Pick first non-forbidden list
    (generatePairs people)                      -- From 1000 shuffled lists
    where is_forbidden (a, b) =                 -- A person cannot be paired:
              name a == name b ||                  -- with themselves
              (name a, name b) `elem` forbidden || -- with a forbidden pairing
              (name b, name a) `elem` forbidden

{- Generates an infinite list of lists of randomly-paired Persons from `people`
 -}
generatePairs :: RandomGen g => [Person] -> Rand g [[(Person, Person)]]
generatePairs people =
    fmap                             -- Do in the Rand g Monad
    (map $ zip people)               -- Pair up shuffled lists of people
    (mapM shuffleM $ repeat people)  -- Create lists of shuffled people forever

{- Action which prints out the assignments, and nothing else
 -}
testSantas :: Action
testSantas _ = mapM_ (putStrLn . santaLine) -- Print out line for each pairing
    where santaLine (a, b) =
              concat ["Assigned ", name a, " (", email a,
                      ") their secret santa: ", name b]
{- Action which displays sample contents of emails
-}
testEmails :: Action
testEmails addresses santas = do
    from <- prompt "What is your name? (used in the \"from\" field)"
    mapM_ (fakeEmails addresses from) santas

{- Displays example secret santa email to `gifter`, assigning them `giftee`
 - `from` is what to use in the "Sender" field
 -}
fakeEmails :: MailingAddressBook -> Name -> (Person, Person) -> IO ()
fakeEmails addresses from (gifter, giftee) = do
    putStrLn $ name gifter ++ "'s email would say:"
    putStrLn . indent $ unlines [
        "To: " ++ email gifter,
        "From: " ++ from,
        "Subject: Secret Santa (Shhh!)",
        emailBody addresses (gifter, giftee)
        ]

{- Action which sends out emails to the Santas in `santas`
 -}
sendEmails :: Action
sendEmails addresses santas =
    SMTP.doSMTPSTARTTLS "smtp.gmail.com" $ \connection -> do
        doubleSendGuard -- Make sure we haven't already sent the emails
        from <- prompt "What is your name? (used in the \"from\" field)"
        authenticate connection -- Connect to server
        putStrLn "Sending emails..."
        mapM_ (sendEmail connection addresses from) santas -- Send the emails
        putStrLn "\t...success!"
        writeFile doubleSendGuardFile "" -- Protect against future double-sends

{- Sends a secret santa email to `gifter`, assigning them `giftee`
 - `connection` is the SMTP connection to use
 - `from` is what to use in the "Sender" field
 -}
sendEmail ::
    SMTP.SMTPConnection
    -> MailingAddressBook
    -> Name
    -> (Person, Person)
    -> IO ()
sendEmail connection addresses from (gifter, giftee) = do
    putStrLn $ "\t...sending to " ++ name gifter ++ " (" ++ email gifter ++ ")..."
    SMTP.sendPlainTextMail -- Send email
        (email gifter) -- Recipient
        from -- Sender
        "Secret Santa (Shhh!)" -- Subject
        (T.pack $ emailBody addresses (gifter, giftee)) -- Body
        connection -- SMTP Connection

{- Fills out email body for given MailingAddressBook and (gifter, giftee) pair
 -}
emailBody :: MailingAddressBook -> (Person, Person) -> String
emailBody addresses (gifter, giftee) =
    "Hello " ++ name gifter ++ ",\n" ++
    "Your secret santa giftee is " ++ name giftee ++ ".\n" ++
    maybe "" address_info (Map.lookup (name giftee) addresses) ++
    "Get them something good! 🎅🎄🎁\n"
    where address_info addr = "Their address is:\n" ++ indent addr ++ "\n"

{- Prompts for username and password, then authenticates these credentials
 -}
authenticate :: SMTP.SMTPConnection -> IO ()
authenticate connection = do
    username <- prompt "What is your gmail username?"
    password <- prompt "What is your gmail password?"
    putStrLn "Authenticating..."
    successful <- SMTP.authenticate SMTP.PLAIN username password connection
    if successful -- Make sure authentication was successful
        then putStrLn "\t...success!"
        else die "Authentication failed"

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

{- Prompts user for input using `msg`
 -}
prompt :: String -> IO String
prompt msg = putStr (msg ++ " ") >> hFlush stdout >> getLine

{- Indents a block of text, handling newlines correctly
-}
indent :: String -> String
indent string = '\t': indent' string
    where
        indent' ('\n': xs) = '\n': '\t': indent' xs
        indent' (x: xs) = x: indent' xs
        indent' [] = []

{- Type declarations
 -}
type FileName = String
type Name = String
type Email = String
type MailingAddress = String
type ForbiddenPair = (Name, Name)
type Santas = [(Person, Person)]
type Action = MailingAddressBook -> Santas -> IO ()
type MailingAddressBook = Map.Map Name MailingAddress

data Person = Person {
    name :: Name,
    email :: Email
} deriving Show

{- Allow Persons to be parsed from CSV records
 -}
instance Csv.FromRecord Person where
    parseRecord record
        | V.length record == 2 =
              Person <$> record .! 0 <*> record .! 1
        | otherwise = mzero

{- Boring command line parsing stuff below this point! -}

{- Datatype representing command-line options
 -}
data Options = Options {
    santaAction :: Action,
    peopleFile :: FileName,
    forbiddenPairsFile :: Maybe FileName,
    mailingAddressFile :: Maybe FileName
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
            command "test-assignments" (
                info (pure testSantas) (progDesc
                "assign secret santa pairings, but do not send emails")
            )
         <> command "test-emails" (
                info (pure testEmails) (progDesc
                "assign secret santa pairings and print fake email messages")
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
    <*> optional (
            strOption (
            long "mailing-addresses"
            <> short 'm'
            <> metavar "<mailing addresses file>"
            <> help "CSV file containing mailing addresses in format \
                    \[Name, Address]"
            )
        )
