open Cards

(*** The main game engine for Durak ***)

(* The game engine *)
val repl : state -> command -> string -> unit

(* Interprets user inputs as commands *)
val parse : string -> command