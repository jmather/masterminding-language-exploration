package main

import (
  "fmt"
  "strconv"
  "math/rand"
  "time"
  "strings"
  "os"
  "bufio"
  "regexp"
)

const SOLUTION_LENGTH = 4
const MAX_GUESSES = 10
const GUESS_WRONG = "_"
const GUESS_RIGHT = "X"
const GUESS_PRESENT = "*"
var CHOICES = []string{"A", "B", "C", "D", "E", "F"}

func gameIntro() {
  fmt.Println("Let's play Mastermind! The rules are easy, I promise.")
  fmt.Println("I will pick " + strconv.Itoa(SOLUTION_LENGTH) + " letters out of a possible " + strconv.Itoa(len(CHOICES)) + ".")
  fmt.Println("You will then get " + strconv.Itoa(MAX_GUESSES) + " chances to guess which letters I picked.")
  fmt.Println("For each letter you guess, I will answer as follows:")
  fmt.Println("\t" + GUESS_WRONG + " means you got it completely wrong.")
  fmt.Println("\t" + GUESS_PRESENT + " means you guessed a letter I used, but it's in the wrong position.")
  fmt.Println("\t" + GUESS_RIGHT + " means you guessed that letter right!")
  fmt.Println("")
}

func contains(s []string, e string) bool {
    for _, a := range s { if a == e { return true } }
    return false
}

func generateSolution() string {
  var picked []string
  picked = make([]string, SOLUTION_LENGTH);
  chosen := 0
  rand.Seed(time.Now().UnixNano())

  for chosen < SOLUTION_LENGTH {
    i := rand.Intn(len(CHOICES))
    choice := CHOICES[i]
    if (contains(picked, choice)) {
      continue;
    }
    picked[chosen] = choice
    chosen++
  }

  return strings.Join(picked, "")
}

func isValidGuess(guess string) bool {
  var pattern = "[^" + strings.Join(CHOICES, "") + "]"
  matched, _ := regexp.MatchString(pattern, guess)
  if (matched) {
    return false
  }

  if (len(guess) != SOLUTION_LENGTH) {
    return false
  }

  return true
}

func getGuess() string {
  var guess = askGuess()

  for isValidGuess(guess) == false {
    fmt.Println("We were unable to understand your input. Please enter only the letters of your guess and press enter.")
    guess = askGuess()
  }

  return guess
}

func askGuess() string {
  fmt.Println("Available pegs: " + strings.Join(CHOICES, ", "))
  fmt.Print("Enter your guess (pick " + strconv.Itoa(SOLUTION_LENGTH) + "): ")
  reader := bufio.NewReader(os.Stdin)
  text, _ := reader.ReadString('\n')
  return strings.ToUpper(strings.TrimSpace(text))
}

func analyzeGuess(solution string, guess string) string {
  var answer []string
  answer = make([]string, SOLUTION_LENGTH);


  for i := 0; i < SOLUTION_LENGTH; i++ {
    solution_letter := solution[i:i+1]
    guess_letter := guess[i:i+1]

    if (solution_letter == guess_letter) {
      answer[i] = GUESS_RIGHT
    } else if (strings.Contains(solution, guess_letter)) {
      answer[i] = GUESS_PRESENT
    } else {
      answer[i] = GUESS_WRONG
    }
  }

  return strings.Join(answer, "")
}

func main() {
  gameIntro()

  solution := generateSolution()
  guesses := 0

  for true {
    if (guesses > MAX_GUESSES) {
      fmt.Println("You've ran out of guesses! Better luck next time!")
      break;
    }

    guess := getGuess()

    if (guess == solution) {
      fmt.Println("You've guessed correctly, you win!")
      break;
    }

    analysis := analyzeGuess(solution, guess)

    fmt.Println("Result: " + analysis)

    guesses++
  }
}
