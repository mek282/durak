open Game
open Cards

(*** Draws text interface for Durak game state ***)

(* Uses text-based graphics to print the contents of the player's hand, the
 * current attacks and defenses, the number of cards in each opponent's hand,
 * and the number of cards left in the deck.  Below this display, the string
 * is printed, and the user is prompted for a response, which is returned. *)
val draw : state -> string -> string