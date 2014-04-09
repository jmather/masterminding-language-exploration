import 'dart:io';
import 'dart:math';

var SOLUTION_LENGTH = 4;
var MAX_GUESSES = 10;
var GUESS_WRONG = '_';
var GUESS_RIGHT = 'X';
var GUESS_PRESENT = '*';
var CHOICES = ['A', 'B', 'C', 'D', 'E', 'F'];

// Library code
void gameIntro() {
  stdout.writeln("Let's play Mastermind! The rules are easy, I promise.");
  stdout.writeln("I will pick $SOLUTION_LENGTH letters out of a possible ${CHOICES.length}.");
  stdout.writeln("You will then get $MAX_GUESSES chances to guess which letters I picked.");
  stdout.writeln("For each letter you guess, I will answer as follows:");
  stdout.writeln("\t" + GUESS_WRONG + " means you got it completely wrong.");
  stdout.writeln("\t" + GUESS_PRESENT + " means you guessed a letter I used, but it's in the wrong position.");
  stdout.writeln("\t" + GUESS_RIGHT + " means you guessed that letter right!");
  stdout.writeln("");
}

String generateSolution() {
  var random = new Random();
  var picked = [];

  while (picked.length < SOLUTION_LENGTH) {
    var choice = CHOICES[random.nextInt(CHOICES.length)];

    if (picked.contains(choice)) {
      continue;
    }

    picked.add(choice);
  }

  return picked.join('');
}

String getGuess() {
  var matcher = new RegExp("[^${CHOICES.join('')}]");
  var guess = askGuess();

  while(matcher.hasMatch(guess) || guess.length != SOLUTION_LENGTH) {
    stdout.writeln("We were unable to understand your input. Please enter only the letters of your guess and press enter.");
    guess = askGuess();
  }

  return guess;
}

String askGuess() {
  stdout.writeln("Available pegs: ${CHOICES.join(', ')}");
  stdout.write("Enter your guess (pick $SOLUTION_LENGTH): ");
  return stdin.readLineSync().trim().toUpperCase();
}

String analyzeGuess(String solution, String guess) {
  var result = [];

  for (var i = 0; i < SOLUTION_LENGTH; i++) {
    var solution_letter = solution.substring(i, i+1);
    var guess_letter = guess.substring(i, i+1);

    if (solution_letter == guess_letter) {
      result.add(GUESS_RIGHT);
    } else if (solution.contains(guess_letter)) {
      result.add(GUESS_PRESENT);
    } else {
      result.add(GUESS_WRONG);
    }
  }

  return result.join('');
}

// Game code
void main(List<String> args) {
  gameIntro();

  var solution = generateSolution();
  var guesses = 0;

  while (true) {
    if (guesses > MAX_GUESSES) {
      stdout.writeln("You've ran out of guesses! Better luck next time!");
      break;
    }

    var guess = getGuess();

    if (guess == solution) {
      stdout.writeln("You've guessed correctly, you win!");
      break;
    }

    var analysis = analyzeGuess(solution, guess);

    stdout.writeln("Result: $analysis");

    guesses++;
  }
}