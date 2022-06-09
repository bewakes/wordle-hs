module WordDisplay(displayChar, GuessState(..))
where

import System.IO
import Data.Char
import System.Console.ANSI

data GuessState = Correct | Present | Absent | None
    deriving (Show, Eq)


disp :: Char -> [SGR] -> IO ()
disp '\n' sgr = putStr "\n"
disp c sgr = do
    setSGR sgr
    putStr (' ': c:" ")
    setSGR [Reset]

displayChar :: GuessState -> Char -> IO ()
displayChar Correct c = disp c [SetColor Foreground Dull White, SetColor Background Dull Green]
displayChar Present c = disp c [SetColor Foreground Vivid White, SetColor Background Dull Yellow]
displayChar Absent c = disp c [SetColor Foreground Dull White, SetColor Background Vivid Black]
displayChar None c = disp c []
