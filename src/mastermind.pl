#!/usr/bin/env perl -w

use strict;
use warnings;

our $solution_length = 4;
our $max_guesses = 10;
our $guess_right = "X";
our $guess_present = "*";
our $guess_wrong = "_";
our $choices = "ABCDEF";

sub gameIntro() {
    print "Let's play Mastermind! The rules are easy, I promise.\r\n";
    print "I will pick $solution_length letters out of a possible " . length($choices) . ".\r\n";
    print "You will then get " . $max_guesses . " chances to guess which letters I picked.\r\n";
    print "For each letter you guess, I will answer as follows:\r\n";
    print "\t" . $guess_wrong . " means you got it completely wrong.\r\n";
    print "\t" . $guess_present . " means you guessed a letter I used, but it's in the wrong position.\r\n";
    print "\t" . $guess_right . " means you guessed that letter right!\r\n";
    print "\r\n";
}

sub generateSolution() {
    my $solution = "";

    while(length($solution) < $solution_length) {
        my $index = rand(length($choices));
        my $choice = substr $choices, $index, 1;

        unless ($solution =~ /${choice}/) {
            $solution = $solution . $choice;
        }
    }

    return $solution;
}

sub askGuess() {
    my $nice_choices = join(", ", split(//, $choices));
    print "Available pegs: " . $nice_choices . "\n";
    print "Enter your guess (pick $solution_length): ";
    my $guess = <STDIN>;
    $guess =~ s/^\s+//;
    $guess =~ s/\s+$//;
    return uc($guess);
}

sub getGuess() {
    my $pattern = "[^$choices]";
    my $temp_guess = askGuess();

    while ($temp_guess =~ /$pattern/ || length($temp_guess) != $solution_length) {
        print "We were unable to understand your input. Please enter only the letters of your guess and press enter.\n";
        $temp_guess = askGuess();
    }

    return $temp_guess;
}

sub analyzeGuess {
    my $solution = $_[0];
    my $guess = $_[1];
    my $result = "";

    for (my $i = 0; $i < $solution_length; $i++) {
        my $solution_letter = substr $solution, $i, 1;
        my $guess_letter = substr $guess, $i, 1;

        if ($solution_letter eq $guess_letter) {
            $result = $result . $guess_right;
        } elsif ($solution =~ /$guess_letter/) {
            $result = $result . $guess_present;
        } else {
            $result = $result . $guess_wrong;
        }
    }

    return $result;
}

gameIntro();

my $solution = generateSolution();
my $guesses = 0;

while(1 == 1) {
    if ($guesses > $max_guesses) {
        print "You've ran out of guesses! Better luck next time!\n";
        last;
    }

    my $guess = getGuess();

    if ($guess eq $solution) {
        print "You've guessed correctly, you win!\n";
        last;
    }

    print "Result: " . analyzeGuess($solution, $guess) . "\n";

    $guesses++;
}