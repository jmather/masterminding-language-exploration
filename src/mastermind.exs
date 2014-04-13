defmodule Mastermind do
	def solution_length, do: 4
	def max_guesses, do: 10
	def guess_wrong, do: "_"
	def guess_right, do: "X"
	def guess_present, do: "*"
	def choices, do: "ABCDEF"

	def game_intro() do
		IO.puts "Let's play Mastermind! The rules are easy, I promise."
		IO.write "I will pick " <> (solution_length |> to_string) <> " letters "
		IO.puts "out of a possible " <> (choices |> String.length |> to_string) <> "."
		IO.puts "You will then get " <> (max_guesses |> to_string) <> " chances to guess which letters I picked."
		IO.puts "For each letter you guess, I will answer as follows:"
		IO.puts "\t" <> guess_wrong <> " means you got it completely wrong."
		IO.puts "\t" <> guess_present <> " means you guessed a letter I used, but it's in the wrong position."
		IO.puts "\t" <> guess_right <> " means you guessed that letter right!"
		IO.puts ""
	end

	def generate_solution() do
		<<a :: size(32), b :: size(32),  c :: size(32)>> = :crypto.strong_rand_bytes(12)
		:random.seed a, b, c

		{ :ok, char_list } = String.to_char_list choices
		result = generate_solution solution_length, char_list, []
		{ :ok, result_string } = String.from_char_list result

		result_string
	end

	def generate_solution(0, _, acc), do: Enum.reverse(acc)
	def generate_solution(remaining, choices_remaining, acc) do
		index = (choices_remaining |> Enum.count |> :random.uniform) - 1
		choice = Enum.at choices_remaining, index
		next_choices = List.delete_at choices_remaining, index
		generate_solution (remaining - 1), next_choices, [choice | acc]
	end

	def ask_guess() do
		IO.puts "Available Pegs: " <> choices
		guess = IO.gets "Enter your guess (pick " <> (solution_length |> to_string) <> "): "
		guess |> String.upcase |> String.strip
	end

	def is_valid_guess?(guess) do
		{ :ok, regex } = Regex.compile "[^" <> choices <> "]"
		
		if (guess |> String.length) != solution_length do
			false
		else
			if Regex.match? regex, guess do
				false
			else
				true
			end
		end
	end

	def get_guess() do
		guess = ask_guess
		if is_valid_guess? guess do
			guess
		else
			IO.puts "We were unable to understand your input. Please enter only the letters of your guess and press enter."
			get_guess
		end
	end

	def analyze_guess(solution, guess) do
		{ :ok, solution_list } = String.to_char_list solution
		{ :ok, guess_list } = String.to_char_list guess

		result_list = analyze_guess solution_list, solution_list, guess_list, []
		{ :ok, result_string } = String.from_char_list result_list
		
		result_string
	end

	def analyze_guess(_, [], _, acc), do: Enum.reverse acc
	def analyze_guess(_, _, [], acc), do: Enum.reverse acc
	def analyze_guess(solution, [ solution_letter | solution_left ], [ guess_letter | guess_left ], acc) do
		if solution_letter == guess_letter do
			analyze_guess solution, solution_left, guess_left, [ guess_right | acc ]
		else
			if Enum.member? solution, guess_letter do
				analyze_guess solution, solution_left, guess_left, [ guess_present | acc ]
			else
				analyze_guess solution, solution_left, guess_left, [ guess_wrong | acc ]
			end
		end
	end

	def run_game() do
		game_intro
		run_game max_guesses, generate_solution
	end

	def run_game(0, _), do: IO.puts "You've ran out of guesses! Better luck next time!"
	def run_game(guesses, solution) do
		guess = get_guess
		if guess == solution do
			IO.puts "You've guessed correctly! You win!"
		else
			analysis = analyze_guess solution, guess
			IO.puts "Result: " <> analysis
			run_game (guesses - 1), solution
		end
	end
end

Mastermind.run_game