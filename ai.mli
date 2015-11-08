open Game
open Cards

(*Gives an AI's response to a first attack, in which it may have the
* option to take, pass, or deflect*)
val first_attack_response : state -> card -> command

(*Gives an AI's response to subsequent attacks, in which it no longer
* has the option to deflect*)
val attack_response : state -> card list -> command list

(*When it is the AI's turn to launch an initial attack, returns the card
* the AI will attack with.*)
val attack : state -> player -> card

(*When another player is being attacked by all players, returns the cards
* the AI will use to attack.*)
val sec_attack : state -> player -> card list

(* When it is an AI's turn to make a move, returns the AI's move as a
* command. *)
val response : state -> command