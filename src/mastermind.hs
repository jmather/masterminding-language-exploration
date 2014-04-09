import System.Random
import System.IO.Unsafe

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
    putStrLn ("")
    putStrLn ("Let's play Mastermind! The rules are easy, I promise.")
    putStrLn ("I will pick " ++ (show solutionLength) ++ " letters out of a possible " ++ (show (length choices)) ++ ".")
    putStrLn ("You will then get " ++ (show maxGuesses) ++ " chances to guess which letters I picked.")
    putStrLn ("For each letter you guess, I will answer as follows:")
    putStrLn ("\t" ++ guessWrong ++ " means you got it completely wrong.")
    putStrLn ("\t" ++ guessPresent ++ " means you guessed a letter I used, but it's in the wrong position.")
    putStrLn ("\t" ++ guessRight ++ " means you guessed that letter right!")
    putStrLn ""


fixChar :: [Char] -> Char
fixChar [val] = val

randomStr :: [Char]
randomStr = take 10 $ randomRs ('A','F') (mkStdGen 3) :: [Char]

generateSolution len acc
    | (length acc) == len = acc
    | len > 0 = do
        strList <- randomStr
        generateSolution len (randomStr ++ acc)
    | otherwise = generateSolution solutionLength []

main :: IO StdGen -> IO ()
main = do
    gameIntro
    putStrLn generateSolution newStdGen
