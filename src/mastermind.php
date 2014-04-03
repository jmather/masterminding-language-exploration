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