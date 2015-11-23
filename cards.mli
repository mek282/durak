(*** Types to be used in Durak ***)

(* Represents the common suits of a card game as a Variant *)
type suit = | Heart | Club | Spade | Diamond

(* Represents a state of player.
 * Human is the human player interacting with the game through the repl
 * CPU of [int] is the computer intelligence defined by Ai, with difficulty set
 * to [int]. Higher [int] represents a harder difficulty. *)
type player_state = | Human | CPU of int

(* card = [suit] * [int] represents a common playing card with
 * suit = [suit] and value = [int]. Values range from 6 - 14, with Jacks,
 * Queens, Kings, and Aces as 11, 12, 13, and 14 respectively. *)
type card = suit * int

(* A list of cards with maximum length 52 *)
type deck = card list

(* Represents a player of the game. The state distinguishes between human
 * and CPU players. hand determines the deck of cards each player holds. *)
type player = { state : player_state;
                hand : deck;
                name: string
              }

val print_card : card -> unit

val print_deck : suit -> deck -> unit

val print_suit : suit -> unit
