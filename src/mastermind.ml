(* Build command: corebuild -package Str mastermind.native && ./mastermind.native *)
#load "str.cma";;

open Core.Std

let _ = Random.self_init()
let (|>) x f = f x

let solution_length = 4
let max_guesses = 10
let guess_right = 'X'
let guess_present = '*'
let guess_wrong = '_'
let choices = "ABCDEF"

let game_intro =
    Printf.printf ("Let's play Mastermind! The rules are easy, I promise.\r\n");
    Printf.printf "I will pick %d letters out of a possible %d.\r\n" solution_length (String.length choices);
    Printf.printf "You will then get %d chances to guess which letters I picked.\r\n" max_guesses;
    Printf.printf "For each letter you guess, I will answer as follows:\r\n";
    Printf.printf "\t%c means you got it completely wrong.\r\n" guess_wrong;
    Printf.printf "\t%c means you guessed a letter I used, but it's in the wrong position.\r\n" guess_present;
    Printf.printf "\t%c means you guessed that letter right!\r\n" guess_right;
    Printf.printf "\r\n"

let generate_solution =
    let solution = ref "" in
    while ((String.length !solution) < solution_length) do
        let pick = Random.int(String.length choices) in
        let letter = String.get choices pick in
        let alreadyPicked = String.contains !solution letter in
            if alreadyPicked = false then solution := String.concat [!solution; String.make 1 letter]
    done;
    !solution


let ask_guess () =
    printf "Available pegs: %s\r\n" choices;
    printf "Enter your guess (pick %d): " solution_length;
    read_line() |> String.strip |> String.uppercase

let is_valid_guess guess =
    if ((String.length guess) = solution_length) then
        let pattern = "[^" ^ choices ^ "]" in
        let regex = Str.regexp pattern in
        let hasMatch = Str.string_match regex guess 0 in
        if (hasMatch) then false else true
    else
        false

let get_guess () =
    let guess = ref (ask_guess()) in
    let isValid = ref (is_valid_guess !guess) in
    while (!isValid = false) do
        printf "We were unable to understand your input. Please enter only the letters of your guess and press enter.\r\n";
        guess := ask_guess();
        isValid := is_valid_guess !guess;
    done;
    !guess

let analyze_guess solution guess =
    let analysis = ref "" in
    for i = 1 to solution_length do
        let solution_letter = String.get solution (i - 1) in
        let guess_letter = String.get guess (i - 1) in
        let ch2str = (fun ch -> String.make 1 ch) in
        let ($$) = (fun str ch -> str ^ (ch2str ch)) in
        if solution_letter = guess_letter then
            analysis := !analysis $$ guess_right
        else if (String.contains solution guess_letter) then
            analysis := !analysis $$ guess_present
        else
            analysis := !analysis $$ guess_wrong
    done;
    !analysis

let rec start_game solution guesses =
    match guesses with
    | 0 -> printf "You've ran out of guesses! Better luck next time!\r\n"
    | _ ->
        let guess = get_guess() in
        if (guess = solution) then printf "You've guessed correctly, you win!\r\n"
        else begin
            printf "Result: %s\r\n" (analyze_guess solution guess);
            start_game solution (guesses - 1)
        end
let () =
    game_intro;
    let solution = generate_solution in
    start_game solution max_guesses
