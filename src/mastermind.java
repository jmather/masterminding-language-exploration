import java.io.*;
import java.lang.Object;
import java.lang.Override;
import java.lang.String;
import java.lang.System;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Random;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class mastermind {
    static int SOLUTION_LENGTH = 4;
    static int MAX_GUESSES = 10;
    static String GUESS_WRONG = "_";
    static String GUESS_RIGHT = "X";
    static String GUESS_PRESENT = "*";
    static String[] CHOICES = {"A", "B", "C", "D", "E", "F"};

    public static void main(String[] args) {
        mastermind game = new mastermind();
        game.run();
    }

    public void print(String output) {
        System.out.println(output);
    }

    protected String arrayToString(String[] arr) {
        String str = "";

        for (String item: arr) {
            str += item;
        }

        return str;
    }

    protected void gameIntro() {
        print("Let's play Mastermind! The rules are easy, I promise.");
        print("I will pick " + String.valueOf(SOLUTION_LENGTH) + " letters out of a possible " + String.valueOf(CHOICES.length) + ".");
        print("You will then get " + String.valueOf(MAX_GUESSES) + " chances to guess whcih letters I picked.");
        print("For each letter you guess, I will answer as follows:");
        print("\t" + GUESS_WRONG + " means you got it completely wrong.");
        print("\t" + GUESS_PRESENT + " means you guessed a letter I used, but it's in the wrong position.");
        print("\t" + GUESS_RIGHT + " means you guessed that letter right!");
        print("");
    }

    protected String generateSolution() {
        ArrayList<String> picked = new ArrayList<String>();
        Random r = new Random();
        String response = "";

        while (picked.size() < SOLUTION_LENGTH) {
            int pos = r.nextInt(CHOICES.length);
            String choice = CHOICES[pos];
            if (picked.contains(choice)) {
                continue;
            }
            picked.add(choice);
        }

        return arrayToString(picked.toArray(new String[picked.size()]));
    }

    protected String getGuess() {
        Pattern p = Pattern.compile("[^"+arrayToString(CHOICES) + "]");
        String guess = askGuess();
        Matcher m = p.matcher(guess);
        while (m.matches() || guess.length() != SOLUTION_LENGTH) {
            print("We were unable to understand your input. Please enter only the letters of your guess and press enter.");
            guess = askGuess();
            m = p.matcher(guess);
        }
        return guess;
    }

    protected String askGuess() {
        String pegs = "";

        for (String pick: CHOICES) {
            pegs += pick;
        }

        print("Available pegs: " + pegs);
        System.out.print("Enter your guess (pick " + String.valueOf(SOLUTION_LENGTH) + "): ");
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));

        try {
            return br.readLine().toString().trim().toUpperCase();
        } catch (IOException ioe) {
            return "";
        }
    }

    protected String analyzeGuess(String solution, String guess) {
        String result = "";
        for (int i = 0; i < SOLUTION_LENGTH; i++) {
            String solution_letter = solution.substring(i, i + 1);
            String guess_letter = guess.substring(i, i + 1);

            if (solution_letter.equals(guess_letter)) {
                result += GUESS_RIGHT;
            } else if (solution.contains(guess_letter)) {
                result += GUESS_PRESENT;
            } else {
                result += GUESS_WRONG;
            }
        }

        return result;
    }

    public void run() {
        gameIntro();

        String solution = generateSolution();
        int guesses = 0;

        while (true) {
            if (guesses > MAX_GUESSES) {
                print("You've ran out of guesses! Better luck next time!");
                break;
            }

            String guess = getGuess();

            if (guess.equals(solution)) {
                print("You've guessed correctly, you win!");
                break;
            }

            String analysis = analyzeGuess(solution, guess);

            print("Result: " + analysis);

            guesses++;
        }
    }
}