#!/usr/bin/env python
import random

# Set up our game
SOLUTION_LENGTH = 4
MAX_GUESSES = 10
GUESS_WRONG = '_'
GUESS_RIGHT = 'X'
GUESS_PRESENT = '*'
CHOICES = ['A', 'B', 'C', 'D', 'E', 'F']

# Library code
def game_intro():
    print "Let's play Mastermind! The rules are easy, I promise."
    print "I will pick " + str(SOLUTION_LENGTH) + " letters out of a possible " + str(len(CHOICES)) + "."
    print "You will then get " + str(MAX_GUESSES) + " chances to guess which letters I picked."
    print "For each letter you guess, I will answer as follows:"
    print "\t" + str(GUESS_WRONG) + " means you got it completely wrong."
    print "\t" + str(GUESS_PRESENT) + " means you guessed a letter I used, but it's in the wrong position."
    print "\t" + str(GUESS_RIGHT) + " means you guessed that letter right!"
    print ""

def generate_solution():
    choices = list(CHOICES)
    picked = []

    while len(picked) < SOLUTION_LENGTH:
        choice = random.choice(choices)
        choices.remove(choice)
        picked.append(choice)

    return ''.join(picked)

def get_guess():
    import re
    pattern = re.compile('[^' + ''.join(CHOICES) + ']')
    guess = ask_guess()

    while pattern.match(guess) or len(guess) != SOLUTION_LENGTH:
        print "We were unable to understand your input. Please enter only the letters of your guess and press enter."
        guess = ask_guess()

    return guess

def ask_guess():
    print "Available pegs: " + ', '.join(CHOICES)
    guess = raw_input("Enter your guess (pick " + str(SOLUTION_LENGTH) + "): ")
    return guess.upper().strip()

def analyze_guess(solution, guess):
    result = []

    for i in range(SOLUTION_LENGTH):
        solution_letter = solution[i]
        guess_letter = guess[i]

        if solution_letter == guess_letter:
            result.append(GUESS_RIGHT)
        elif guess_letter in solution:
            result.append(GUESS_PRESENT)
        else:
            result.append(GUESS_WRONG)

    return ''.join(result)

# Game code
game_intro()

solution = generate_solution()
guesses = 0

while True:
    if guesses > MAX_GUESSES:
        print "You've ran out of guesses! Better luck next time!"
        break

    guess = get_guess()

    if guess == solution:
        print "You've guessed correctly, you win!"
        break

    analysis = analyze_guess(solution, guess)

    print "Result: " + analysis

    guesses += 1
