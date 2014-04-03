#!/usr/bin/env php
<?php

require __DIR__.'/mastermind-lib.php';

// Set up our game
define('SOLUTION_LENGTH', 4);
define('MAX_GUESSES', 10);
define('GUESS_WRONG', '_');
define('GUESS_RIGHT', 'X');
define('GUESS_PRESENT', '*');

$choices = array('A', 'B', 'C', 'D', 'E', 'F');

// Library code
function gameIntro($choices) {
    echo "Let's play MasterMind! The rules are easy, I promise.\r\n";
    echo "I will pick " . SOLUTION_LENGTH . " letters out of a possible " . count($choices).".\r\n";
    echo "You will then get " . MAX_GUESSES . " chances to guess which letters I picked.\r\n";
    echo "For each letter you guess, I will answer as follows:\r\n";
    echo "\t" . GUESS_WRONG . " means you got it completely wrong.\r\n";
    echo "\t" . GUESS_PRESENT . " means you guessed a letter I used, but it's in the wrong position.\r\n";
    echo "\t" . GUESS_RIGHT . " means you guessed that letter right!\r\n";
    echo "\r\n";
}

function generateSolution($choices)
{
    $picks = array_rand($choices, SOLUTION_LENGTH);
    return array_reduce($picks, function($acc, $key) use ($choices) { return $acc . $choices[$key]; });
}

function getGuess($choices)
{
    $allowed_match = '/[^' . implode('', $choices) . ']/';
    $guess = askGuess($choices);
    while (preg_match($allowed_match, $guess) || strlen($guess) != SOLUTION_LENGTH) {
        echo "We were unable to understand your input. Please enter only the letters of your guess and press enter.\r\n";
        $guess = askGuess($choices);
    }
    return $guess;
}

function askGuess($choices) {
    echo 'Available pegs: ' . implode(', ', $choices) . "\r\n";
    $guess = readline('Enter your guess (pick ' . SOLUTION_LENGTH . '): ');
    return trim(strtoupper($guess));
}

function analyzeGuess($solution, $guess) {
    $result = '';

    for ($i = 0; $i < SOLUTION_LENGTH; $i++) {
        $solution_letter = substr($solution, $i, 1);
        $guess_letter = substr($guess, $i, 1);

        if ($solution_letter == $guess_letter) {
            $result .= GUESS_RIGHT;
        } elseif (strpos($solution, $guess_letter) > -1) {
            $result .= GUESS_PRESENT;
        } else {
            $result .= GUESS_WRONG;
        }
    }

    return $result;
}


// Begin game
gameIntro($choices);

$solution = generateSolution($choices);
$guesses = 0;

while (true) {
    if ($guesses > MAX_GUESSES) {
        echo "You've ran out of guesses! Better luck next time!\r\n";
        break;
    }

    $guess = getGuess($choices);

    if ($guess == $solution) {
        echo "You've guessed correctly, you win!\r\n";
        break;
    }

    $analysis = analyzeGuess($solution, $guess);

    echo "Result: $analysis\r\n";

    $guesses++;
}