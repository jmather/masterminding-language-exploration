#!/usr/bin/env ruby

# Set up our game
SOLUTION_LENGTH = 4
MAX_GUESSES = 10
GUESS_WRONG = '_'
GUESS_RIGHT = 'X'
GUESS_PRESENT = '*'
CHOICES = %w(A B C D E F)

def game_intro
  print "Let's play Mastermind! The rules are easy, I promise.\r\n"
  print "I will pick " + SOLUTION_LENGTH.to_s + " letters out of a possible " + CHOICES.size.to_s + ".\r\n"
  print "You will then get " + MAX_GUESSES.to_s + " chances to guess which letters I picked.\r\n"
  print "For each letter you guess, I will answer as follows:\r\n"
  print "\t" + GUESS_WRONG.to_s + " means you got it completely wrong.\r\n"
  print "\t" + GUESS_PRESENT.to_s + " means you guessed a letter I used, but it's in the wrong position.\r\n"
  print "\t" + GUESS_RIGHT.to_s + " means you guessed that letter right!\r\n"
  print "\r\n"
end

def generate_solution
  CHOICES.sample(SOLUTION_LENGTH).join('')
end

def get_guess
  regex = Regexp.new('^[' + CHOICES.join('') + ']+$')
  guess = ask_guess
  until regex.match(guess) and guess.size == SOLUTION_LENGTH
    puts 'We were unable to understand your input. Please enter only the letters of your guess and press enter.'
    guess = ask_guess
  end
  guess
end

def ask_guess
  puts 'Available pegs: ' + CHOICES.join(', ')
  print 'Enter your guess (pick ' + SOLUTION_LENGTH.to_s + '): '
  gets.strip.upcase
end

def analyze_guess(solution, guess)
  result = ''
  for index in (0..solution.size - 1) do
    solution_letter = solution.slice(index, 1)
    guess_letter = guess.slice(index, 1)

    if solution_letter == guess_letter
      result += GUESS_RIGHT
    elsif solution.count(guess_letter) > 0
      result += GUESS_PRESENT
    else
      result += GUESS_WRONG
    end
  end
  result
end

game_intro

solution = generate_solution
guesses = 0

while true
  if guesses > MAX_GUESSES
    puts "You've ran out of guesses! Better luck next time!"
    break
  end

  guess = get_guess

  if guess == solution
    puts "You've guessed correctly, you win!"
    break
  end

  analysis = analyze_guess(solution, guess)

  puts "Result: #{analysis}"

  guesses += 1
end