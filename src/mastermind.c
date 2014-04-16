#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

static const int SOLUTION_LENGTH = 4;
static const int MAX_GUESSES = 10;
static const char GUESS_RIGHT = 'X';
static const char GUESS_PRESENT = '*';
static const char GUESS_WRONG = '_';
static const char CHOICES[] = "ABCDEF";


void gameIntro() {
    printf("Let's play Mastermind! The rules are easy, I promise.\r\n");
    printf("I will pick %d letters out of a possible %d.\r\n", SOLUTION_LENGTH, (int) sizeof(CHOICES) - 1);
    printf("You will then get %d chances to guess which letters I picked.\r\n", MAX_GUESSES);
    printf("For each letter you guess, I will answer as follows:\r\n");
    printf("\t%c means you got it completely wrong.\r\n", GUESS_WRONG);
    printf("\t%c means you guessed a letter I used, but it's in the wrong position.\r\n", GUESS_PRESENT);
    printf("\t%c means you guessed that letter right!\r\n", GUESS_RIGHT);
    printf("\r\n");
}

void generateSolution(char *solution, size_t length) {
    int count = 0;
    size_t index;
    char choice;
    char *s;
    char *full_solution;
    full_solution = solution;

    *solution = 0;

    // first answer was always 5 -- throwing it way
    index = (double) rand() / RAND_MAX * (sizeof CHOICES - 1);
    while(count < length) {
        index = (double) rand() / RAND_MAX * (sizeof CHOICES - 1);
        choice = CHOICES[index];
        s = strchr(full_solution, choice);
        if (s == NULL) {
            *solution = choice;
            solution++;
            *solution = 0;
            count++;
        }
    }
}

char *askGuess(int length) {
    char line[1024];
    char *eof;
    char *response = malloc(length + 2);
    char *response_bookmark;
    response_bookmark = response;

    line[0] = 0;
    line[sizeof(line)-1] = 1;

    printf("Available pegs: %s\n", CHOICES);
    printf("Enter your guess (pick %d): ", SOLUTION_LENGTH);
    eof = fgets(line, sizeof(line), stdin);

    for (int i = 0; i < length; i++) {
        *response_bookmark = toupper(line[i]);
        response_bookmark++;
    }

    // we copy one extra character to see if they typed in too much...
    *response_bookmark = line[length + 1];
    response_bookmark++;

    // all dogs go to heaven, and all c strings end in null.
    *response_bookmark = 0;

    return response;
}

bool isValidGuess(char *guess) {
    char *s;
    char *guess_bookmark;
    guess_bookmark = guess;

    for (int i = 0; i < SOLUTION_LENGTH; i++) {
        s = strchr(CHOICES, *guess_bookmark);
        if (s == NULL) {
            return false;
        }
        guess_bookmark++;
    }

    if (*guess_bookmark != 0) {
        return false;
    }

    return true;
}

char *getGuess(int length) {
    char *guess = askGuess(length);

    while (isValidGuess(guess) == false) {
        printf("We were unable to understand your input. Please enter only the letters of your guess and press enter.\r\n");
        free(guess);
        guess = askGuess(length);
    }

    return guess;
}

char *analyzeGuess(char *solution, char *guess) {
    char *result = malloc(SOLUTION_LENGTH + 1);
    char *result_bookmark;
    char *solution_bookmark;
    char *search;
    solution_bookmark = solution;
    result_bookmark = result;

    for (int i = 0; i < SOLUTION_LENGTH; i++) {
        search = strchr(solution_bookmark, *guess);

        if (*solution == *guess) {
            *result = GUESS_RIGHT;
        } else if (search != NULL) {
            *result = GUESS_PRESENT;
        } else {
            *result = GUESS_WRONG;
        }

        result++;
        guess++;
        solution++;
    }

    *result = 0;
    return result_bookmark;
}

int main( int argc, const char* argv[] )
{
    srand(time(NULL));

    gameIntro();

    char *solution = (char *)malloc((SOLUTION_LENGTH+1)*sizeof(char));
    generateSolution(solution, SOLUTION_LENGTH);
    int guesses = 0;
    char *guess;

    while(true) {
        if (guesses > MAX_GUESSES) {
            printf("You've ran out of guesses! Better luck next time!\r\n");
            break;
        }

        guess = getGuess(SOLUTION_LENGTH);

        if (strcmp(solution, guess) == 0) {
            printf("You've guessed correctly, you win!\r\n");
            break;
        }

        printf("Result: %s\n", analyzeGuess(solution, guess));

        guesses++;
    }
}

