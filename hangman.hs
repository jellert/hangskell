import System.Environment
import System.Directory
import System.IO
import Data.List
import Data.Char
import System.Random

data GameState = Loss | InProgress | Win

lives = 10

main :: IO ()
main = do
    putStrLn "Welcome to Hangman!"
    putStrLn "Loading game..."
    fileShort   <- readFile "short.txt"
    fileMed     <- readFile "med.txt"
    fileLong    <- readFile "long.txt"
    let short   = lines fileShort
        med     = lines fileMed
        long    = lines fileLong
    newGame long med short

newGame :: [String] -> [String] -> [String] -> IO ()
newGame easy med hard = do
    putStrLn "New Game!"
    (Just difficulty)  <- difficultySelect
    putStrLn $ "Selected " ++ difficulty ++" difficulty."
    let pool =  if difficulty == "Easy" then easy else
                if difficulty == "Medium" then med else hard
    gen <- getStdGen
    let (index, newGen) = randomR (0, length pool) gen
        word = map toUpper (pool !! index)
        guesses = ""
    putStrLn word
    -- putStrLn (blanks "COMPUTER" "CE")
    (state, newGuesses) <- processGuess word guesses
    
    putStrLn "Okay" 

difficultySelect :: IO (Maybe String)
difficultySelect = do
    putStrLn "Select Difficulty: (E)asy, (M)edium, or (H)ard?"
    difficulty <- getChar
    return $ verifyDifficulty difficulty

processGuess :: String -> String -> IO (GameState, String)
processGuess word guessed = do
    putStrLn $ "Word: " ++ blanks word guessed
    putStrLn $ "Previous incorrect guesses: "
    putStrLn $ (flatten $ zip (guessedWrong word guessed) (repeat ' '))
    prompt "Guess: "
    guess <- getChar
    let guess'      = toUpper guess
        toPrint     =   if guess' `elem` guessed then "Already guessed this." else
                        if guess' `elem` word then "Correct!" else "Wrong."
        guessed'    =   if guess' `elem` guessed then guessed else guess':guessed
        state       =   if (takeOutSpaces $ blanks word guessed') == word then Win else
                        if length guessed' == lives then Loss else InProgress
        lastAction  = case state of Win         -> return (Win, guessed')
                                    Loss        -> return (Loss, guessed')
                                    InProgress  -> processGuess word guessed'
    putStrLn toPrint
    lastAction

--  returns difficulty level if input is valid
--  TODO: error?
verifyDifficulty :: Char -> (Maybe String)
verifyDifficulty 'e'    = Just "Easy"
verifyDifficulty 'E'    = Just "Easy"
verifyDifficulty 'm'    = Just "Medium"
verifyDifficulty 'M'    = Just "Medium"
verifyDifficulty 'h'    = Just "Hard"
verifyDifficulty 'H'    = Just "Hard"
verifyDifficulty _      = Nothing

--  returns hangman blanks representation of the word
--  for example, if word is computer and c and e have been guessed,
--  blanks "COMPUTER" "CE" = "C _ _ _ _ _ E _ "
blanks :: String -> String -> String
blanks word guessed = flatten $ zip blnks spaces
                    where   blnks = map (`conditionalBlank` guessed) word
                            spaces = repeat ' '

--  helper that returns _ if char not guessed, otherwise id
conditionalBlank :: Char -> String -> Char
conditionalBlank c guessed = if c `elem` guessed then c else '_'

--  helper that generates list of incorrect guesses given word and list of guesses
guessedWrong :: String -> String -> String
guessedWrong word guessed = [ x | x <- guessed, not $ x `elem` word]

--  flattens list of char pairs into string (which is output of zip of 2 strings)
flatten :: [(Char, Char)] -> String
flatten pairs = foldr (\(x,s) acc -> x:s:acc) "" pairs

takeOutSpaces :: String -> String
takeOutSpaces = filter (/= ' ')

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine
