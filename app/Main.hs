module Main where

import System.IO
import Control.Monad.State
import Debug.Trace
import System.Console.ANSI
import Data.Char
import Data.List
import System.Random

import WordDisplay

type WordleState = (String, Int)
type CurrResult = [GuessState]

data GameState = GameState
    { actualWord :: String
    , attempts :: Int
    , currentGuess :: String
    , currentResult :: CurrResult
    }
    deriving Show

maxGuess = 6

type CorrectWord = String

data GuessChar = GuessChar
    { char:: Char
    , gstate :: GuessState
    }
    deriving (Show, Eq)

type Guess = [GuessChar]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n a = take n a: chunksOf n (drop n a)

mkGuess :: String -> Guess
mkGuess = map (`GuessChar` None)

checkGreens :: String -> Guess -> Guess
checkGreens = zipWith checkGreen'
    where checkGreen' c gc@(GuessChar g s)
            | c == g = GuessChar g Correct
            | otherwise = gc


-- TODO: maybe memoize
count :: (Eq a) => a -> [a] -> Int
count _ [] = 0
count a (x:xs)
  | a == x = 1 + count a xs
  | otherwise = count a xs


checkYellow :: CorrectWord -> Guess -> GuessChar -> Guess
checkYellow word gs g@(GuessChar c s)
  | s == Correct = g:gs
  | otherwise = let charCount = count c word
                    guessCount = count c (map char gs)
                 in if charCount == 0
                       then GuessChar c Absent:gs
                       else
                           if charCount > guessCount
                              then GuessChar c Present:gs
                              else GuessChar c Absent:gs

check :: CorrectWord -> String -> Guess
check word guess = reverse $ foldl (checkYellow word) [] (checkGreens word (mkGuess guess))


getNewAlpha :: Guess -> Guess -> Guess
getNewAlpha alpha result = map (update result) alpha
    where update [] gs = gs
          update (x: xs) gs@(GuessChar c _)
            | char x == c = if gstate x == Absent
                               then GuessChar c Absent
                               else if gstate x == Correct
                                   then GuessChar c Correct
                                   else gs
            | otherwise = update xs gs


displayGuess (GuessChar c s) = displayChar s c
nline = GuessChar '\n' None

showAlphaGuesses :: [[GuessChar]] -> Guess -> IO ()
showAlphaGuesses guesses alpha = do
    let alphachunks = chunksOf 5 alpha
    mapM_ displayGuess (intercalate [nline] alphachunks)
    putStrLn "\n==============="
    mapM_ displayGuess (intercalate [nline] guesses)
    putStrLn ""

prompt guesses = do
    putStr $ "Enter guess[" ++ show (length guesses + 1) ++ "/" ++ show maxGuess ++ "]: "

validateWord :: String -> [String] -> Either String String
validateWord guess allWords
  | length guess /= 5 = Left "Must be 5 lettered word!"
  | guess `notElem` allWords = Left "NOT A VALID WORD!"
  | otherwise = Right guess

playGame :: [String] -> String -> IO ()
playGame allWords word = do
    let alpha = mkGuess "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        guesses = []
    putStrLn ""
    showAlphaGuesses guesses alpha
    playGame' guesses alpha
    where playGame' guesses alpha
            | length guesses >= 6 = putStrLn $ "\nOops!! The word was: " ++ word
            | otherwise = do
                prompt guesses
                guess <- map toUpper <$> getLine
                case validateWord guess allWords of
                  Left err -> do
                       clearScreen
                       showAlphaGuesses guesses alpha
                       putStrLn err
                       playGame' guesses alpha
                  _ -> do
                    let result = check word guess
                        newAlpha = getNewAlpha alpha result
                        gs = reverse (result:guesses)
                    if guess == word
                       then do
                           mapM_ displayGuess (intercalate [nline] gs)
                           putStrLn "\nBrilliant!!   \n"
                       else do
                         clearScreen
                         showAlphaGuesses gs newAlpha
                         playGame' gs newAlpha

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  content <- readFile "data/words.txt"
  let allWords = map (map toUpper) $ words content
  i <- randomRIO (0, length allWords - 1)
  playGame allWords $ allWords !! i
