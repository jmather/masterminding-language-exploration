open Core.Std

val solution_length : int
val max_guesses : int
val guess_right : char
val guess_present : char
val guess_wrong : char
val choices : string

val game_intro : unit
val generate_solution : string
val ask_guess : unit -> string
val is_valid_guess : string -> bool
val get_guess : unit -> string
val analyze_guess : string -> string -> string
val start_game : string -> int -> unit