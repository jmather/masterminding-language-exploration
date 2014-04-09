#!/usr/bin/env node
'use strict';

var SOLUTION_LENGTH = 4;
var MAX_GUESSES = 10;
var GUESS_WRONG = '_';
var GUESS_RIGHT = 'X';
var GUESS_PRESENT = '*';
var CHOICES = ['A', 'B', 'C', 'D', 'E', 'F'];

// Library code
function gameIntro() {
    console.log("Let's play Mastermind! The rules are easy, I promise.\r\n");
    console.log("I will pick " + SOLUTION_LENGTH + " letters out of a possible " + CHOICES.length + ".");
    console.log("You will then get " + MAX_GUESSES + " chances to guess which letters I picked.");
    console.log("For each letter you guess, I will answer as follows:");
    console.log("\t" + GUESS_WRONG + " means you got it completely wrong.");
    console.log("\t" + GUESS_PRESENT + " means you guessed a letter I used, but it's in the wrong position.");
    console.log("\t" + GUESS_RIGHT + " means you guessed that letter right!");
    console.log("");
}

function generateSolution() {
    var selected = [];

    while(selected.length < SOLUTION_LENGTH) {
        var pos = Math.floor((Math.random()*choices.length));
        var picked = CHOICES[pos];

        if (selected.contains(picked)) {
            continue;
        }

        selected.push(picked);
    }

    return selected.join('');
}

function getGuess(callback) {
    var regex = new RegExp("^[" + CHOICES.join('') + ']+$');

    askGuess(function(guess) {
        if (regex.test(guess) && guess.length === SOLUTION_LENGTH) {
            callback(guess);
        } else {
            console.log("We were unable to understand your input. Please enter only the letters of your guess and press enter.");
            getGuess(callback);
        }
    });
}

function askGuess(callback) {
    console.log("Available pegs: " + CHOICES.join(', '));
    var readline = require('readline');
    var io = readline.createInterface({
        input: process.stdin,
        output: process.stdout,
        terminal: false
    });

    io.question("Enter your guess (pick " + SOLUTION_LENGTH + "): ", function(answer) {
        callback(answer.trim().toUpperCase());
    });
}

function analyzeGuess(solution, guess) {
    var result = [];

    for (var i = 0; i < SOLUTION_LENGTH; i++) {
        var solution_letter = solution.substring(i, i+1);
        var guess_letter = guess.substring(i, i+1);

        if (solution_letter == guess_letter) {
            result.push(GUESS_RIGHT);
        } else if (solution.indexOf(guess_letter) > -1) {
            result.push(GUESS_PRESENT);
        } else {
            result.push(GUESS_WRONG);
        }
    }

    return result.join('');
}

// Game code
gameIntro();

var solution = generateSolution();
var guesses = 0;

var gameFunc = function (guesses_remaining) {
    if (guesses_remaining < 1) {
        console.log("You've ran out of guesses! Better luck next time!");
        return;
    }

    getGuess(function(guess) {
        if (guess === solution) {
            console.log("You've guessed correctly, you win!");
            return;
        }

        var analysis = analyzeGuess(solution, guess);

        console.log("Result: " + analysis);

        gameFunc(guesses_remaining - 1);
    });
};

gameFunc(MAX_GUESSES);