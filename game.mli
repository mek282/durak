open Cards

(*** The main game engine for Durak ***)

(* Representation of player moves.
  - Attack (Heart, 6) represents an attack with a Six of Hearts
  - Defend ((Heart, 6), (Heart, 8)) represents a player defending
    Attack (Heart, 6) with an Eight of Hearts
  - Take represents a player choosing to take the cards on the table rather
    than defend.
  - Pass represents a player passing when given the opportunity to attack
    the current defender.
  - Deflect ((Six, Diamonds), (Six, Hearts)) represents a player choosing
    to pass along an Attack (Six, Diamonds) to the next player by adding
    their own Six of Hearts.
*)
type command = | Attack of card | Defend of (card * card) | Take | Pass
               | Deflect of (card * card)


(* Creates a randomized 52-card deck in which each card is different.
 * The suit of the last card, which is the trump suit, is stored in the pair *)
val init_deck : unit -> (suit * deck)

(* Creates an initial state for the game in which all cards have been passed
 * out. init_game_state 1 2 3 creates a game of 4 players with an AI of
 * each difficulty level (numbered 1 for easy, 2 for medium, 3 for hard.) *)
val init_game_state : int -> int -> int -> state

(* Interprets user inputs as commands *)
val parse : string -> command

(* Calls itself recursively to update the state in response to commands *)
val repl : state -> command -> unit