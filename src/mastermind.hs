import System.Random
import Data.List
import System.IO.Unsafe
import System.IO
import qualified Data.Text as T
import Text.Regex.Posix

-- These are actually functions
solutionLength = 4
maxGuesses = 10
guessWrong = "_"
guessRight = "X"
guessPresent = "*"
choices = "ABCDEF"

-- Library code
gameIntro :: IO ()
gameIntro = do
    -- Normally, " " (space) has the highest precendent, and so this executes fine
    putStrLn "Let's play Mastermind! The rules are easy, I promise."
    -- If we start doing other actions, we use $ to say "evaluate everything to the right first"
    -- Without this, the next line would be executed as: (putStrLn "I will pick ") ++ "something"
    -- With a $, it becomes then: putStrLn ("a" ++ "b")
    putStrLn $ "I will pick " ++ (show solutionLength) ++ " letters out of a possible " ++ (show (length choices)) ++ "."
    putStrLn $ "You will then get " ++ (show maxGuesses) ++ " chances to guess which letters I picked."
    putStrLn $ "For each letter you guess, I will answer as follows:"
    putStrLn $ "\t" ++ guessWrong ++ " means you got it completely wrong."
    putStrLn $ "\t" ++ guessPresent ++ " means you guessed a letter I used, but it's in the wrong position."
    putStrLn $ "\t" ++ guessRight ++ " means you guessed that letter right!"
    putStrLn ""


-- If real Haskell programmers see this function, I am pretty sure they will cry
randomStr :: (RandomGen g) => g -> [Char]
randomStr g = do
    take 1 $ randomRs ('A','F') $ unsafePerformIO newStdGen :: [Char]


-- <function name> :: <type constraints> => <signature>
generateSolutionWorker :: (Num a, Eq a) => a -> [Char] -> StdGen -> [Char]
generateSolutionWorker 0 acc@(h:t) g = acc
generateSolutionWorker n acc g
    | letter `isInfixOf` acc = generateSolutionWorker n acc g
    | otherwise = generateSolutionWorker (n - 1) (letter ++ acc) g
    where letter = (randomStr g)

generateSolution :: StdGen -> [Char]
generateSolution g = generateSolutionWorker solutionLength "" g


isValidGuess :: String -> Bool
isValidGuess guess
    | isTotalMatch = True
    | otherwise = False
    where pat = "[^" ++ (choices) ++ "]"
          isTotalMatch = isMatch == False && rightLength == True
          isMatch = (guess =~ pat) :: Bool
          rightLength = (length guess) == (solutionLength)


getGuess :: IO String
getGuess = do
    guess <- askGuess
    if (isValidGuess guess)
        then return guess
        else do
            putStrLn "We were unable to understand your input. Please enter only the letters of your guess and press enter."
            getGuess

askGuess :: IO String
askGuess = do
    let choiceList = [(head choices)] ++ (concat [", " ++ [x] | x <- (tail choices)])
    putStrLn ("Available pegs: " ++ choiceList)
    putStr ("Enter your guess (pick " ++ (show solutionLength) ++ "): ")
    hFlush stdout
    line <- getLine
    -- . joins functions T.unpack . T.toUpper $ <var> == (T.unpack (T.toUpper <var>))
    return $ T.unpack . T.toUpper . T.strip $ T.pack line


analyzeGuessWorker :: String -> String -> String -> String -> String
analyzeGuessWorker _ [] _ acc = reverse acc
analyzeGuessWorker _ _ [] acc = reverse acc
analyzeGuessWorker solution (solutionLetter : solutionRest) (guessLetter : guessRest) acc
    | isSame = analyzeGuessWorker solution solutionRest guessRest (guessRight ++ acc)
    | isInSolution = analyzeGuessWorker solution solutionRest guessRest (guessPresent ++ acc)
    | otherwise = analyzeGuessWorker solution solutionRest guessRest (guessWrong ++ acc)
    where isSame = guessLetter == solutionLetter
          isInSolution = [guessLetter] `isInfixOf` solution


analyzeGuess :: String -> String -> String
analyzeGuess [] _ = []
analyzeGuess _ [] = []
analyzeGuess solution guess = analyzeGuessWorker solution solution guess []

-- Game code
startGame :: StdGen -> IO ()
startGame g = do
    gameIntro
    let solution = (generateSolution g)
    gameLoop solution maxGuesses


gameLoop :: String -> Integer -> IO ()
gameLoop solution 0 = putStrLn "You've ran out of guesses! Better luck next time!"
gameLoop solution iteration = do
    guess <- getGuess
    if guess == solution
        then putStrLn "You've guessed correctly! You win!"
        else do
            putStrLn ("Result: " ++ (analyzeGuess solution guess))
            gameLoop solution (iteration - 1)


main:: IO ()
main = do
    g <- newStdGen
    startGame g