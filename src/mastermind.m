## Compile with: gcc -framework Foundation mastermind.m -o mastermind && ./mastermind
#import <Foundation/Foundation.h>
#include <stdlib.h>

static int const SOLUTION_LENGTH = 4;
static int const MAX_GUESSES = 10;
static NSString * const GUESS_RIGHT = @"X";
static NSString * const GUESS_PRESENT = @"*";
static NSString * const GUESS_WRONG = @"_";
static NSString * const CHOICES = @"ABCDEF";

void gameIntro ()
{
    printf("Let's play Mastermind! The rules are easy, I promise.\r\n");
    printf("I will pick %d letters out of a possible %d.\r\n", SOLUTION_LENGTH, (int) [CHOICES length]);
    printf("You will then get %d chances to guess which letters I picked.\r\n", MAX_GUESSES);
    printf("For each letter you guess, I will answer as follows:\r\n");
    printf("\t%s means you got it completely wrong.\r\n", [GUESS_WRONG UTF8String]);
    printf("\t%s means you guessed a letter I used, but it's in the wrong position.\r\n", [GUESS_PRESENT UTF8String]);
    printf("\t%s means you guessed that letter right!\r\n", [GUESS_RIGHT UTF8String]);
    printf("\r\n");
}

NSString *generateSolution ()
{
	NSMutableString *result = [NSMutableString stringWithString: @""];
	NSString *choice;
	int index;
	NSRange match;


	while ([result length] < SOLUTION_LENGTH)
	{
        index = arc4random() % [CHOICES length];
		choice = [CHOICES substringWithRange: NSMakeRange (index, 1)];
		match = [result rangeOfString: choice];

		if (match.location == NSNotFound) {
			[result appendString: choice];
		}
	}

	return result;
}

NSString *askGuess ()
{
    printf("Available pegs: %s\n", [CHOICES UTF8String]);
    printf("Enter your guess (pick %d): ", SOLUTION_LENGTH);

	char str[50] = {0};
	scanf("%s", str);                    // read and format into the str buffer

	NSString *guess = [NSString stringWithUTF8String:str];

	NSString *trimmedGuess = [guess stringByTrimmingCharactersInSet:
	                                  [NSCharacterSet whitespaceCharacterSet]];
	
	return [trimmedGuess uppercaseString];
}

BOOL isValidGuess (NSString *guess)
{
	if ([guess length] != SOLUTION_LENGTH)
	{
		return NO;
	}

	NSString *pattern = [NSString stringWithFormat:@"[^%s]", [CHOICES UTF8String]];
	NSError  *error = nil;

	NSRegularExpression* regex = [NSRegularExpression regularExpressionWithPattern:pattern options:0 error:&error];

	if ([regex numberOfMatchesInString:guess options:0 range:NSMakeRange(0, [guess length])] > 0)
	{
		return NO;
	}

	return YES;
}

NSString *getGuess ()
{
	NSString *guess = askGuess();

	while (isValidGuess(guess) == NO)
	{
        printf("We were unable to understand your input. Please enter only the letters of your guess and press enter.\r\n");
        guess = askGuess();
	}

	return guess;
}

NSString *analyzeGuess (NSString *solution, NSString *guess)
{
	NSMutableString *result = [NSMutableString stringWithString: @""];;
	NSString *solution_letter = nil;
	NSString *guess_letter = nil;
	NSRange match;

	for (int i = 0; i < SOLUTION_LENGTH; i++)
	{
		solution_letter = [solution substringWithRange: NSMakeRange (i, 1)];
		guess_letter = [guess substringWithRange: NSMakeRange (i, 1)];
		match = [solution rangeOfString: guess_letter];
		if (solution_letter == guess_letter) {
			[result appendString: GUESS_RIGHT];
		} else if (match.location != NSNotFound) {
			[result appendString: GUESS_PRESENT];
		} else {
			[result appendString: GUESS_WRONG];
		}
	}

	return result;
}

int main (int argc, const char * argv[])
{
	NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

	gameIntro();

	NSString *solution = generateSolution();
	NSString *guess;
	NSString *analysis;
	int guesses = 0;

	while (true)
	{
		if (guesses > MAX_GUESSES)
		{
            printf("You've ran out of guesses! Better luck next time!\r\n");
        	break;
		}

		guess = getGuess();

		if ([guess isEqualToString:solution] == YES)
		{
			printf("You've guessed correctly, you win!\r\n");
            break;
		}

		analysis = analyzeGuess(solution, guess);

		printf("Result: %s\n", [analysis UTF8String]);

		guesses++;
	}

	[pool drain];
	return 0;
}
