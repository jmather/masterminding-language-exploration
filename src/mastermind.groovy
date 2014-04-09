#!/usr/bin/env groovy
SOLUTION_LENGTH = 4
MAX_GUESSES = 10
GUESS_WRONG = '_'
GUESS_RIGHT = 'X'
GUESS_PRESENT = '*'
CHOICES = ['A', 'B', 'C', 'D', 'E', 'F']

def gameIntro() {
    println("Let's play Mastermind! The rules are easy, I promise.")
    println("I will pick " + SOLUTION_LENGTH + " letters out of a possible " + CHOICES.size + ".")
    println("You will then get " + MAX_GUESSES + " chances to guess which letters I picked.")
    println("For each letter you guess, I will answer as follows:")
    println("\t" + GUESS_WRONG + " means you got it completely wrong.")
    println("\t" + GUESS_PRESENT + " means you guessed a letter I used, but it's in the wrong position.")
    println("\t" + GUESS_RIGHT + " means you guessed that letter right!")
    println("")
}

def generateSolution() {
    def picked = []
    def random = new Random()
    while (picked.size() < SOLUTION_LENGTH) {
        def rnd = random.nextInt(CHOICES.size())
        def choice = CHOICES[rnd]
        if (picked.contains(choice)) {
            continue;
        }

        picked.add(choice)
    }

    return picked.join('')
}

def getGuess() {
    def choices = CHOICES.join("")
    def pattern = ~"[^$choices]"
    def guess = askGuess()
    while (pattern.matcher(guess).find() || guess.length() != SOLUTION_LENGTH) {
        println("We were unable to understand your input. Please enter only the letters of your guess and press enter.")
        guess = askGuess()
    }

    return guess
}

def askGuess() {
    println("Available pegs: " + CHOICES.join(", "))
    guess = System.console().readLine("Enter your guess (pick " + SOLUTION_LENGTH + "): ")
    return guess.trim().toUpperCase()
}

def analyzeGuess(String solution, String guess) {
    def result = []

    for (def i = 0; i < SOLUTION_LENGTH; i++) {
        def solution_letter = solution.substring(i, i + 1)
        def guess_letter = guess.substring(i, i + 1)

        if (solution_letter == guess_letter) {
            result.add(GUESS_RIGHT)
        } else if (solution.contains(guess_letter)) {
            result.add(GUESS_PRESENT)
        } else {
            result.add(GUESS_WRONG)
        }
    }

    return result.join("")
}


gameIntro()

solution = generateSolution()
guesses = 0

while (true) {
    if (guesses > MAX_GUESSES) {
        println("You've ran out of guesses! Better luck next time!")
        break;
    }

    guess = getGuess()

    if (guess == solution) {
        println("You've guessed correctly, you win!")
        break;
    }

    def analysis = analyzeGuess(solution, guess)

    println("Result: " + analysis)

    guesses++
}