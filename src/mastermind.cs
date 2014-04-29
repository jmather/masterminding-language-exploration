using System;
using System.Text.RegularExpressions;
 
public class Mastermind
{
	static public int SOLUTION_LENGTH = 4;
	static public int MAX_GUESSES = 10;
	static public char GUESS_RIGHT = 'X';
	static public char GUESS_PRESENT = '*';
	static public char GUESS_WRONG = '_';
	static public string CHOICES = "ABCDEF";

	public void GameIntro() {
	    Console.WriteLine("Let's play Mastermind! The rules are easy, I promise.");
	    Console.WriteLine("I will pick " + SOLUTION_LENGTH + " letters out of a possible " + CHOICES.Length + ".");
	    Console.WriteLine("You will then get " + MAX_GUESSES + " chances to guess which letters I picked.");
	    Console.WriteLine("For each letter you guess, I will answer as follows:");
	    Console.WriteLine("\t" + GUESS_WRONG + " means you got it completely wrong.");
	    Console.WriteLine("\t" + GUESS_PRESENT + " means you guessed a letter I used, but it's in the wrong position.");
	    Console.WriteLine("\t" + GUESS_RIGHT + " means you guessed that letter right!");
	    Console.WriteLine("");

	}

	public string GenerateSolution() {
		Random rnd = new Random();
		string solution = "";
		while (solution.Length < SOLUTION_LENGTH) {
			int pick = rnd.Next(CHOICES.Length);
			char choice = CHOICES[pick];
			if (solution.IndexOf(choice) == -1) {
				solution = solution + choice;
			}
		}

		return solution;
	}

	public string AskGuess() {
		Console.WriteLine("Available pegs: " + CHOICES);
		Console.Write("Enter your guess (pick " + SOLUTION_LENGTH + "): ");
		string guess = Console.ReadLine();
		return guess.Trim().ToUpper();
	}

	public bool IsGuessValid(string guess) {
		if (guess.Length != SOLUTION_LENGTH) {
			return false;
		}

		string pattern = "[^" + CHOICES + "]";

		if (Regex.IsMatch(guess, pattern)) {
			return false;
		}

		return true;
	}

	public string GetGuess() {
		string guess = AskGuess();
		while (IsGuessValid(guess) == false) {
			Console.WriteLine("We were unable to understand your input. Please enter only the letters of your guess and press enter.");
			guess = AskGuess();
		}

		return guess;
	}

	public string AnalyzeGuess(string solution, string guess) {
		string result = "";

		for (int i = 0; i < SOLUTION_LENGTH; i++) {
			char solution_letter = solution[i];
			char guess_letter = guess[i];

			if (solution_letter == guess_letter) {
				result = result + GUESS_RIGHT;
			} else if (solution.IndexOf(guess_letter) > -1) {
				result = result + GUESS_PRESENT;
			} else {
				result = result + GUESS_WRONG;
			}
		}

		return result;
	}

	public void StartGame() {
		GameIntro();

		string solution = GenerateSolution();
		int guesses = 0;

		while(true) {
			if (guesses > MAX_GUESSES) {
				Console.WriteLine("You've ran out of guesses! Better luck next time!");
				return;
			}

			string guess = GetGuess();

			if (guess == solution) {
				Console.WriteLine("You've guessed correctly, you win!");
				return;
			}

			Console.WriteLine("Result: " + AnalyzeGuess(solution, guess));

			guesses = guesses + 1;
		}
	}

	static public void Main ()
	{
		Mastermind game = new Mastermind();
		game.StartGame();
	}
 
}
