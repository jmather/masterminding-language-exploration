-module(mastermind).

-export([start_game/0]).

-define(SOLUTION_LENGTH, 4).
-define(MAX_GUESSES, 10).
-define(GUESS_WRONG, "_").
-define(GUESS_RIGHT, "X").
-define(GUESS_PRESENT, "*").
-define(CHOICES, "ABCDEF").


game_intro() ->
  io:format("Let's play Mastermind! The rules are easy, I promise.~n"),
  io:format("I will pick ~B letters out of a possible ~B.~n", [?SOLUTION_LENGTH, length(?CHOICES)]),
  io:format("You will then get ~B chances to guess which letters I picked.~n", [?MAX_GUESSES]),
  io:format("For each letter you guess, I will answer as follows:~n"),
  io:format("  ~s means you got it completely wrong.~n", [?GUESS_WRONG]),
  io:format("  ~s means you guessed a letter I used, but it's in the wrong position.~n", [?GUESS_PRESENT]),
  io:format("  ~s means you guessed that letter right!~n", [?GUESS_RIGHT]),
  io:format("~n").


generate_solution() ->
  <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
  random:seed(A, B, C),
  generate_solution(?SOLUTION_LENGTH, ?CHOICES, []).

generate_solution(0, _, Acc) -> Acc;
generate_solution(Remaining, Choices, Acc) ->
  Position = random:uniform(length(Choices)),
  Choice = lists:nth(Position, Choices),
  NewChoices = lists:delete(Choice, Choices),
  generate_solution(Remaining - 1, NewChoices, [Choice | Acc]).


is_valid_guess(Guess) ->
  GuessRegex = "[^" ++ ?CHOICES ++ "]",
  case re:run(Guess, GuessRegex) of
    nomatch ->
      if
        length(Guess) == ?SOLUTION_LENGTH -> true;
        true -> false
      end;
    _ -> false
  end.


get_guess() ->
  Guess = ask_guess(),
  case is_valid_guess(Guess) of
    true -> Guess;
    false ->
      io:format("We were unable to understand your input. Please enter only the letters of your guess and press enter.~n"),
      get_guess()
  end.

ask_guess() ->
  ChoiceDisplay = [hd(?CHOICES) | lists:flatmap(fun(X) -> [", ", X] end, tl(?CHOICES))],
  io:format("Available pegs: ~s~n", [ChoiceDisplay]),
  Prompt = "Enter your guess, (pick " ++ integer_to_list(?SOLUTION_LENGTH) ++ "): ",
  Guess = io:get_line(Prompt),
  TempGuess = string:strip(string:to_upper(Guess)),
  string:left(TempGuess, length(TempGuess) - 1).


analyze_guess(Solution, Guess) ->
  analyze_guess(Solution, Solution, Guess, []).

analyze_guess(_, [], [], Acc) ->
  lists:reverse(Acc);
analyze_guess(Solution, [GuessChar | SolTail], [GuessChar | GuessTail], Acc) ->
  analyze_guess(Solution, SolTail, GuessTail, [?GUESS_RIGHT | Acc]);
analyze_guess(Solution, [_ | SolTail], [GuessChar | GuessTail], Acc) ->
  case lists:member(GuessChar, Solution) of
    true -> analyze_guess(Solution, SolTail, GuessTail, [?GUESS_PRESENT | Acc]);
    false -> analyze_guess(Solution, SolTail, GuessTail, [?GUESS_WRONG | Acc])
  end.


start_game() ->
  game_intro(),
  game_loop(generate_solution(), ?MAX_GUESSES).


game_loop(_, 0) ->
  io:format("You've ran out of guesses! Better luck next time!~n");
game_loop(Solution, GuessesRemaining) ->
  Guess = get_guess(),
  if
    Guess == Solution ->
      io:format("You've guessed correctly! You win!~n");
    true ->
      Analysis = analyze_guess(Solution, Guess),
      io:format("Result: ~s~n", [Analysis]),
      game_loop(Solution, GuessesRemaining - 1)
  end.
