open Cards

(*** Draws text interface for Durak game state ***)

(* Prints the title*)
val draw_title: unit -> unit

(* Prints the title screen with instructions *)
val draw_title_screen: unit -> unit

(* Prints the win screen *)
val draw_win: unit -> unit

(* Prints the lose screen *)
val draw_lose: unit -> unit

(* Uses text-based graphics to print the contents of the player's hand, the
 * current attacks and defenses, the number of cards in each opponent's hand,
 * and the number of cards left in the deck. The user prompt and response will
 * be processed *)
val draw : state -> unit
