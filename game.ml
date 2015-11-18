open Cards

(* Used in parsing to identify invalid user inputs *)
exception Cannot_parse of string

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

(* Breaks a string into two parts, where the first is everything before
 * the first space (not including leading whitespace), and the second is
 * everything after that space, or none if nothing follows.  Leading and
 * trailing whitespace are removed *)
let split (s : string) : (string * string option) =
  let str = String.trim s in
  if String.contains str ' '
  then let i = String.index str ' ' in
    let fst = String.sub str 0 i in
    let lst = String.sub str i (String.length str - i) in
    let lst' = String.trim lst in
    (fst , Some lst')
  else (str, None)

(* Takes in a string of form "[rank]" and returns the corresponding integer
 * The rank may be a number, such as "7", a number word, like "ten", or a
 * facecard rank, like "king"
 * precondition: all letters must be lowercase. The string must be composed
 * of either one number in range [6,10], a word corresponding to one of those
 * numbers, or one of the following: "jack", "queen", "king", "ace" *)
let parse_rank (s : string) : int =
  match s with
  | "jack" -> 11
  | "queen" -> 12
  | "king" -> 13
  | "ace" -> 14
  | "six" -> 6
  | "seven" -> 7
  | "eight" -> 8
  | "nine" -> 9
  | "ten" -> 10
  | "6" -> 6
  | "7" -> 7
  | "8" -> 8
  | "9" -> 9
  | "10" -> 10
  | _ -> raise (Cannot_parse "rank")

(* Takes in a string of form "[Suit]" and returns a suit object *)
let parse_suit (s : string) : suit =
  failwith "unimplemented"

(* Takes in a string of form "[Rank] of [Suit]", returns corresponding card.
 * Example strings include: "Ace of Spades", "12 of Hearts", "Six of Clubs"
 * Case does not matter. Numbers may be written out in letters or digits.*)
let parse_card (s : string) : card =
  failwith "unimplemented"

(* Interprets user inputs as commands. Case does not matter. *)
let parse (s : string) : command =
  (*let s' = String.lowercase s in*)
  failwith "unimplemented"

(* Calls itself recursively to update the state in response to commands *)
let repl g c =
  failwith "unimplemented"

(*TEST CASES*)

let test_split () =
  let a = "one two" in
  let a' = ("one", Some "two") in
  let b = "  one   two    " in
  let c = "  one     " in
  let c' = ("one", None) in
  let d = "one" in
  let e = "one two three" in
  let e' = ("one", Some "two three") in
  let f = "   one     two three  " in
  assert (split a = a');
  assert (split b = a');
  assert (split c = c');
  assert (split d = c');
  assert (split e = e');
  assert (split f = e');
  ()

let test_parse_rank () =
  assert (parse_rank "ace" = 14);
  assert (parse_rank "queen" = 12);
  assert (parse_rank "king" = 13);
  assert (parse_rank "jack" = 11);
  assert (parse_rank "ten" = 10);
  assert (parse_rank "nine" = 9);
  assert (parse_rank "eight" = 8);
  assert (parse_rank "seven" = 7);
  assert (parse_rank "six" = 6);
  assert (parse_rank "6" = 6);
  assert (parse_rank "10" = 10);
  assert (parse_rank "9" = 9);
  assert (parse_rank "8" = 8);
  assert (parse_rank "7" = 7);
  ()

let run_tests () =
  test_split ();
  test_parse_rank ();
  print_endline "all tests pass";
  ()

let _ = run_tests ()