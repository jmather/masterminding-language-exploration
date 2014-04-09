#!/bin/sh
exec scala "$0" "$@"
!#
import java.util.Random

val SOLUTION_LENGTH = 4
val MAX_GUESSES = 10
val GUESS_WRONG = "_"
val GUESS_RIGHT = "X"
val GUESS_PRESENT = "*"
val CHOICES = Array[String]("A", "B", "C", "D", "E", "F")

// Library code
def gameIntro() {
  println("Let's play Mastermind! The rules are easy, I promise.")
  println("I will pick " + SOLUTION_LENGTH + " letters out of a possible " + CHOICES.length + ".")
  println("You will then get " + MAX_GUESSES + " chances to guess which letters I picked.")
  println("For each letter you guess, I will answer as follows:")
  println("\t" + GUESS_WRONG + " means you got it completely wrong.")
  println("\t" + GUESS_PRESENT + " means you guessed a letter I used, but it's in the wrong position.")
  println("\t" + GUESS_RIGHT + " means you guessed that letter right!")
  println("")
}

def generateSolutionPicker(arr: Array[String], rand: Random): Array[String] = {
  val rnd = rand.nextInt(CHOICES.length)
  val choice = CHOICES(rnd)

  if (arr.length == 0 || arr.contains(choice) == false) {
    return arr :+ choice
  }

  arr
}

def generateSolution(): String = {
  var picked = Array[String]()
  val random = new Random()

  // Why use another function? Because scala has no BREAK
  while (picked.length < SOLUTION_LENGTH) {
    picked = generateSolutionPicker(picked, random)
  }

  picked.mkString("")
}

def isValidGuess(str: String): Boolean = {
  val patternString = ".*[^" + CHOICES.mkString("") + "].*"

  if (str.matches(patternString)) {
    return false
  }

  if (str.length != SOLUTION_LENGTH) {
    return false
  }

  true
}

def getGuess(): String = {
  val guess = askGuess()

  if (!isValidGuess(guess)) {
    println("We were unable to understand your input. Please enter only the letters of your guess and press enter.")
    getGuess()
  }

  guess
}

def askGuess(): String = {
  println("Available pegs: " + CHOICES.mkString(", "))
  val data = readLine("Enter your guess, (pick " + SOLUTION_LENGTH + "): ")
  data.trim.toUpperCase
}

def analyzeGuess(solution: String, guess: String): String = {
  var result = Array[String]()

  for ( i <- 0 to SOLUTION_LENGTH - 1) {
    val solution_letter = solution.substring(i, i + 1)
    val guess_letter = guess.substring(i, i + 1)

    if (solution_letter == guess_letter) {
      result = result :+ GUESS_RIGHT
    } else if (solution.contains(guess_letter)) {
      result = result :+ GUESS_PRESENT
    } else {
      result = result :+ GUESS_WRONG
    }
  }

  result.mkString("")
}

// Game code
object mastermind {
  def main(args: Array[String]) {
    gameIntro()

    val solution = generateSolution()
    var guesses = 0

    while (true) {
      if (guesses > MAX_GUESSES) {
        println("You've ran out of guesses! Better luck next time!")
        return
      }

      val guess = getGuess()

      if (guess == solution) {
        println("You've guessed correctly, you win!")
        return
      }

      val analysis = analyzeGuess(solution, guess)

      println("Result: " + analysis)

      guesses += 1
    }
  }
}

mastermind.main(args);