open Cards

(*** The main game engine for Durak ***)

(* Creates a randomized 36-card deck in which each card is different.
 * The suit of the last card, which is the trump suit, is stored in the pair *)
val init_deck : unit -> (suit * deck)

(* Creates an initial state for the game in which all cards have been passed
 * out. init_game_state 1 2 3 creates a game of 4 players with an AI of
 * each difficulty level (numbered 1 for easy, 2 for medium, 3 for hard.) *)
val init_game_state : string -> int list -> state

(* [step g c] returns the state that results from applying a command c to
 * the game state g and a message describing game events *)
val step : state -> command -> state*string