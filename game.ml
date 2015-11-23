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


(* Prints title and instructions *)
let title_screen () =
  Printf.printf "\n\n  DURAK \n
  Created by Mary Kaminski | Drew Samuels | Ivan Zaitsev | Jose Castro \n
  Welcome to Durak. The object of the game is to get rid of all your cards
  before your opponents do the same. There are no winners, only one loser:
  the 'DURAK', or Idiot!
  You will receive a hand of six cards from a standard deck with all cards
  numbered five or below removed. Aces are high. A random suit will be chosen
  and known as the 'trump suit.'
  When it is your turn to attack the next player, choose a card from your hand
  and type 'attack with [value] of [suit]'.
  The defender may 'deflect' an initial attack by placing down a card of the
  same value: 'deflect [attack value] of [attack suit] with [deflect value] of
  [deflect suit]'.
  Alternatively, the defender may play a card of the same suit as and a higher
  value than the attacking card. In addition, any card of the trump suit can
  defend against any card not of the trump suit. To defend, type 'defend against
  [attack value] of [attack suit] with [defense value] of [defense suit]'.
  The defender's final option is to take all cards on the table. This includes
  those they have already defended. If the defender cannot defend or deflect an
  attack, their only option is to 'take'.
  If the defender chooses to defend against the first attack, then all players
  will have a chance to attack the defender each round until there
  are a maximum of six attacks in play. Each new attacker may only attack with
  cards of the same values already in play. For example, if the initial attack
  is a six of hearts and this is defended with a seven of hearts, the next
  attacker may only attack with either a six or a seven.\n
  Press 'Enter' to begin. \n";
  let s = read_line () in (fun x -> ()) s

let rec unshuffled_deck d n =
  if n < 15
  then let new_d = List.append d [(Heart, n); (Diamond, n);
                                 (Spade, n); (Club, n)] in
        unshuffled_deck new_d (n+1)
  else d

let rec shuffle l1 l2 l3 =
  match l1 with
  | e1::e2::tl -> shuffle tl (e1::l2) (e2::l3)
  | e1::tl -> shuffle tl l2 (e1::l3)
  | [] -> (l2, l3)

let choose_trump () =
  let n = Random.int 4 in
  if n = 0 then Heart
    else if n = 1 then Diamond
    else if n = 2 then Spade
  else Club

(* Creates a randomized 36 card deck in which each card is different. The deck
 * is a standard deck with twos, threes, fours, and fives removed.
 * The suit of the last card, which is the trump suit, is stored in the pair *)
let init_deck _ =
  let d = unshuffled_deck [] 6 in
  let num_shuffles = 7 + (Random.int 20) in
  let rec shuffle_deck n deck = (
    if n >= 0 then
      let shuffled = shuffle deck [] [] in
        if (Random.int 2) = 1 then
          shuffle_deck (n-1) (List.append (snd shuffled) (fst shuffled))
        else
          shuffle_deck (n-1) (List.append (fst shuffled) (snd shuffled))
    else deck ) in
  (choose_trump (), shuffle_deck num_shuffles d)

let deal_hand d =
  match d with
  | c1::c2::c3::c4::c5::c6::tl -> ([c1; c2; c3; c4; c5; c6], tl)
  | _ -> failwith "The deck is too small - only 4 players max!"

(* Creates an initial state for the game in which all cards have been passed
 * out *)
let init_game_state d1 d2 d3 =
  let start_deck = init_deck () in
  let h1 = deal_hand (snd start_deck) in
  let p1 = {state = Human; hand = (fst h1)} in
  let h2 = deal_hand (snd h1) in
  let p2 = {state = CPU d1; hand = (fst h2)} in
  let h3 = deal_hand (snd h2) in
  let p3 = {state = CPU d2; hand = (fst h3)} in
  let h4 = deal_hand (snd h3) in
  let p4 = {state = CPU d3; hand = (fst h4)} in
  { deck = (snd h4);
    trump = (fst start_deck);
    attackers = [p1; p3; p4];
    defender = p2;
    table = [];
    active = p1;
    discard = [];
    winners = [];
  }

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

let test_init_deck () =
  Printf.printf "\n\nDECK 1 TEST:\n\n";
  let d1 = init_deck () in
  print_deck (fst d1) (snd d1);
  Printf.printf "\n\nDECK 2 TEST:\n\n";
  let d2 = init_deck () in
  print_deck (fst d2) (snd d2);
  Printf.printf "\n\nDECK 3 TEST:\n\n";
  let d3 = init_deck () in
  print_deck (fst d3) (snd d3);
  Printf.printf "\n\nDECK 4 TEST:\n\n";
  let d4 = init_deck () in
  print_deck (fst d4) (snd d4);
  Printf.printf "\n\nDECK 5 TEST:\n\n";
  let d5 = init_deck () in
  print_deck (fst d5) (snd d5);
  Printf.printf "\n\nDECK 6 TEST:\n\n";
  let d6 = init_deck () in
  print_deck (fst d6) (snd d6);
  ()

let test_init_game_state () =
  let g1 = init_game_state 1 2 3 in
  assert (List.length (g1.deck) = 12);
  assert ((g1.defender).state = CPU 1);
  assert (List.length ((g1.defender).hand) = 6);
  assert ((g1.active).state = Human);
  assert (List.length ((g1.active).hand) = 6);
  ()

let run_tests () =
  test_split ();
  test_parse_rank ();
  test_parse_suit ();
  test_parse_card ();
  test_parse ();
(*   test_init_deck (); *)
  print_endline "all tests pass";
  ()

let _ =
  title_screen ();
  run_tests ()