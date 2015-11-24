open Cards

(*** Calculate an AI player's moves in response to current game state ***)

(* When it is an AI's turn to make a move, returns the AI's move as a
* command. *)
val response : state -> command

