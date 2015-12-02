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


val print_card : card -> unit

val print_deck : suit -> deck -> unit

val print_suit : suit -> unit

(* Stores information about the state of the game.
 * deck is the cards that haven't been drawn yet
 * trump is the trump suit
 * attackers index 0 primary attacker index lst next to be attacked
 * defender is the player currently being attacked
 * attackers index 0 represents primary attacker, last index represents the
 * next player to be attacked
 * table is a list of pairs where the first element is an "attacking card" and
 * the second element is None or Some "defending card"
 * active is the player whose turn it is
 * discard represents the discard pile
 * winners is the list of players who are out of the game
 * Invariant: no two distinct players in the game may have the same name *)
type state = { deck: deck;
               trump: suit;
               attackers: player list;
               defender: player;
               table: (card * card option) list;
               active: player;
               discard: deck;
               winners: player list;
              }
