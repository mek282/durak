open Cards

(*** The main game engine for Durak ***)

(* The game engine. [repl g c s] will print message s to the human player and
 * update g with command c, then recurse until a "durak" has been chosen *)
val repl : state -> command -> string -> unit

(* Interprets user inputs as commands *)
val parse : string -> command