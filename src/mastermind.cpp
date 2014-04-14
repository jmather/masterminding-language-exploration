#include <string>
#include <iostream>
#include <algorithm>
#include <regex>

const int __SOLUTION_LENGTH__ = 4;
const int __MAX_GUESSES__ = 10;
const std::string __GUESS_WRONG__ = "_";
const std::string __GUESS_RIGHT__ = "X";
const std::string __GUESS_PRESENT__ = "*";
const std::string __CHOICES__ = "ABCDEF";

void gameIntro() {
    std::cout << "Let's play Mastermind! The rules are easy, I promise.\r\n";
    std::cout << "I will pick " << std::to_string(__SOLUTION_LENGTH__) << " letters out of a possible ";
    std::cout << std::to_string(__CHOICES__.length()) << ".\r\n";
    std::cout << "You will then get " << std::to_string(__MAX_GUESSES__) << " chances to guess which letters I picked.\r\n";
    std::cout << "For each letter you guess, I will answer as follows:\r\n";
    std::cout << "\t" << __GUESS_WRONG__ << " means you got it completely wrong.\r\n";
    std::cout << "\t" << __GUESS_PRESENT__ << " means you guessed a letter I used, but it's in the wrong position.\r\n";
    std::cout << "\t" << __GUESS_RIGHT__ << " means you guessed that letter right!\r\n";
    std::cout << "\r\n";
}

std::string generateSolution() {
    int length = __CHOICES__.length();
    std::string result;

    while(result.length() < __SOLUTION_LENGTH__) {
        int rand = std::rand() % length;
        char choice = __CHOICES__[rand];

        if (result.find(choice) == std::string::npos) {
            result += choice;
        }
    }

    return result;
}

// from http://stackoverflow.com/questions/216823/whats-the-best-way-to-trim-stdstring
std::string choppa(const std::string &t, const std::string &ws)
{
    std::string str = t;
    size_t found;
    found = str.find_last_not_of(ws);
    if (found != std::string::npos)
    	str.erase(found+1);
    else
    	str.clear();            // str is all whitespace

    return str;
}

std::string askGuess() {
    std::cout << "Available pegs: " << __CHOICES__.at(0);
    for (int i = 1; i < __CHOICES__.length(); i++) {
        std::cout << ", " << __CHOICES__.at(i);
    }
    std::cout << "\r\n";
    std::cout << "Enter your guess (pick " << std::to_string(__SOLUTION_LENGTH__) << "): ";
    std::string guess;
    std::cin >> guess;
    std::locale loc;
    std::string result = "";
    for (int i = 0; i < guess.length(); i++) {
        result += std::toupper(guess[i], loc);
    }
    return choppa(result, " \r\n\t");
}

std::string getGuess() {
    std::string pattern = "[^" + __CHOICES__ + "]";
    std::string guess = askGuess();
    std::regex rx(pattern);

    while (std::regex_search(guess.begin(), guess.end(), rx) || guess.length() != __SOLUTION_LENGTH__) {
        std::cout << "We were unable to understand your input. Please enter only the letters of your guess and press enter.\r\n";
        guess = askGuess();
    }

    return guess;
}

std::string analyzeGuess(std::string solution, std::string guess) {
    std::string result = "";

    for (int i = 0; i < __SOLUTION_LENGTH__; i++) {
        char solution_letter = solution.at(i);
        char guess_letter = guess.at(i);

        if (solution_letter == guess_letter) {
            result += __GUESS_RIGHT__;
        } else if (solution.find(guess_letter) != std::string::npos) {
            result += __GUESS_PRESENT__;
        } else {
            result += __GUESS_WRONG__;
        }
    }

    return result;
}

int main() {
    gameIntro();

    std::string solution = generateSolution();
    int guesses = 0;
    std::string guess;

    while (true) {
        if (guesses > __MAX_GUESSES__) {
            std::cout << "You've ran out of guesses! Better luck next time!\r\n";
            break;
        }

        guess = getGuess();

        if (guess == solution) {
            std::cout << "You've guessed correctly, you win!\r\n";
            break;
        }

        std::cout << "Result: " << analyzeGuess(solution, guess) << std::endl;

        guesses++;
    }

    return 0;
}