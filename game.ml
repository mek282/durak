open Cards

type command = | Attack of card | Defend of (card * card) | Take | Pass
               | Deflect of (card * card)

type state = { deck: deck;
               trump: suit;
               attackers: player list;
               defender: player;
               table: (card * card option) list;
               active: player;
               discard: deck;
               winners: player list;
              }

(* Creates a randomized 52-card deck in which each card is different.
 * The suit of the last card, which is the trump suit, is stored in the pair *)
let init_deck _ =
  failwith "unimplemented"

(* Creates an initial state for the game in which all cards have been passed
 * out *)
let init_game_state _ =
  failwith "unimplemented"

(* Interprets user inputs as commands *)
let parse s =
  failwith "unimplemented"

(* Calls itself recursively to update the state in response to commands *)
let repl g c =
  failwith "unimplemented"