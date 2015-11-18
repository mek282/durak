open Cards


(* Used in parsing to identify invalid user inputs *)
exception Cannot_parse of string


(* Correspond to user inputs:
 * "Attack with [card]"
 * "Defend against [card] with [card]"
 * "Take"
 * "Pass"
 * "Deflect against [card] with [card]" *)
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


(* Splits a string so that the first three words go in one string and the
 * rest in the string option *)
let split3 (s : string) : (string * string option) =
  match split s with
  | (_,None) -> raise (Cannot_parse s)
  | (w1, Some r) -> (
    match split r with
    | (_,None) -> raise (Cannot_parse s)
    | (w2, Some r2) -> let (w3, r3) = split r2 in (w1^" "^w2^" "^w3,r3)
  )


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
  | _ -> raise (Cannot_parse s)


(* Takes in a string of form "[Suit]" and returns a suit object
 * Precondition: s must be composed only of lowercase letters *)
let parse_suit (s : string) : suit =
  match s with
  | "hearts" -> Heart
  | "clubs" -> Club
  | "spades" -> Spade
  | "diamonds" -> Diamond
  | _ -> raise (Cannot_parse s)


(* Takes in a string of form "[Rank] of [Suit]", returns corresponding card.
 * Example strings include: "Ace of Spades", "12 of Hearts", "Six of Clubs"
 * Case does not matter. Numbers may be written out in letters or digits.*)
let parse_card (s : string) : card =
  match split s with
  | (_ , None) -> raise (Cannot_parse s)
  | (fst, Some snd) -> begin
      match split snd with
      | (_, None) -> raise (Cannot_parse s)
      | (_, Some lst) -> (parse_suit lst, parse_rank fst)
    end


(* Interprets user inputs as commands. Case does not matter. *)
let parse (s : string) : command =
  let str = String.lowercase s in
  match split str with
  | ("take",_) -> Take
  | ("pass",_) -> Pass
  | ("attack",Some s') -> begin
    match split s' with
    | (_,Some c) -> Attack (parse_card c)
    | _ -> raise (Cannot_parse s)
  end
  | (com,Some s') -> begin
    match split s' with (*s' is "against [card] with [card]"*)
    | (_,Some s'') -> ( (*s'' is "[card] with [card]"*)
      match split3 s'' with
      | (c1,Some s''') -> ( (*s''' is "with [card]"*)
        match split s''' with
        | (_,Some c2) -> (
          let c1' = parse_card c1 in
          let c2' = parse_card c2 in
          if com = "defend" then Defend (c1',c2')
          else if com = "deflect" then Deflect (c1',c2')
          else raise (Cannot_parse s)
        )
        | _ -> raise (Cannot_parse s)
      )
      | _ -> raise (Cannot_parse s)
    )
    | _ -> raise (Cannot_parse s)
  end
  | _ -> raise (Cannot_parse s)


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

let test_parse_suit () =
  assert (parse_suit "hearts" = Heart);
  assert (parse_suit "spades" = Spade);
  assert (parse_suit "clubs" = Club);
  assert (parse_suit "diamonds" = Diamond);
  ()

let test_parse_card () =
  assert (parse_card "six of hearts" = (Heart,6));
  assert (parse_card "7 of spades" = (Spade,7));
  assert (parse_card "jack of diamonds" = (Diamond,11));
  assert (parse_card "ace of clubs" = (Club,14));
  ()

let test_parse () =
  let a = "AtTack with 8 of CLUBS" in
  let a' = Attack (Club,8) in
  let b = "take" in
  let b' = Take in
  let c = "Defend  aGAINST ace of hearts with six of spades " in
  let c' = Defend ((Heart,14),(Spade,6)) in
  let d = "   pAss" in
  let d' = Pass in
  let e = " deflect against 10 of diamonds with jack of   hearts " in
  let e' = Deflect ((Diamond,10),(Heart,11)) in
  assert (parse a = a');
  assert (parse b = b');
  assert (parse c = c');
  assert (parse d = d');
  assert (parse e = e');
  ()

let run_tests () =
  test_split ();
  test_parse_rank ();
  test_parse_suit ();
  test_parse_card ();
  test_parse ();
  print_endline "all tests pass";
  ()

let _ = run_tests ()