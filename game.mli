open Cards

type command = | Attack of card | Defend of (card * card) | Take | Pass
               | Deflect of card


(* Stores information about the state of the game.
 * deck is the cards that haven't been drawn yet
 * trump is the trump suit
 * attackers index 0 primary attacker index lst next to be attacked
 * defender is the player currently being attacked
 * table is a list of cards currently being used in attack and their responses*)
type state = {deck: deck; trump: suit; attackers: player list; defender: player;
              table: (card * card option) list}

(* Creates a randomized 52-card deck in which each card is different.
 * The suit of the last card, which is the trump suit, is stored in the pair *)
val init_deck : () -> (suit, deck)

(* Creates an initial state for the game in which all cards have been passed
 * out *)
val init_game_state : () -> state

(* Interprets user inputs as commands *)
val parse : string -> command

(* Calls itself recursively to update the state in response to commands *)
val repl : state -> command -> unit