open Cards
open Command
open Ai


(* Used in parsing to identify invalid user inputs *)
exception Cannot_parse of string
exception Invalid_action of string


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

(*
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

let e_names = ref ["Mr. Camel"; "Camille"; "Cameron"; "Melly"]
let m_names = ref ["Drew"; "Ivan"; "Jose"; "Mary"]
let h_names = ref ["Clarkson"; "Chirag"; "Remy"; "M. George"]

let gen_name d =
  if d = 1
    then let n = List.nth (!e_names) (Random.int (List.length (!e_names))) in
         e_names := List.filter (fun x -> x <> n) (!e_names); n
  else if d = 2
    then let n = List.nth (!m_names) (Random.int (List.length (!m_names))) in
         m_names := List.filter (fun x -> x <> n) (!m_names); n
  else let n = List.nth (!h_names) (Random.int (List.length (!h_names))) in
         h_names := List.filter (fun x -> x <> n) (!h_names); n

(* Creates an initial state for the game in which all cards have been passed
 * out *)
let init_game_state s d1 d2 d3 =
  let start_deck = init_deck () in
  let h1 = deal_hand (snd start_deck) in
  let p1 = {state = Human; hand = (fst h1); name = s} in
  let h2 = deal_hand (snd h1) in
  let p2 = {state = CPU d1; hand = (fst h2); name = (gen_name d1)} in
  let h3 = deal_hand (snd h2) in
  let p3 = {state = CPU d2; hand = (fst h3); name = (gen_name d2)} in
  let h4 = deal_hand (snd h3) in
  let p4 = {state = CPU d3; hand = (fst h4); name = (gen_name d3)} in
  { deck = (snd h4);
    trump = (fst start_deck);
    attackers = [p1; p3; p4];
    defender = p2;
    table = [];
    active = p1;
    discard = [];
    winners = [];
  }
*)

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

(* Some helpers for strings manipulation - Vanya *)
(* returns string representation of a suit *)
let suit_to_string (suit: suit) : string =
  match suit with
  | Heart   -> "Hearts"
  | Club    -> "Clubs"
  | Diamond -> "Diamonds"
  | Spade   -> "Spades"

(* returns string representation of a rank *)
let rank_to_string (rank: int) : string =
  match rank with
  | 14 -> "Ace"
  | 13 -> "King"
  | 12 -> "Queen"
  | 11 -> "Jack"
  | n  -> string_of_int n

(* Finished and tested 11/26 - Vanya *)
(* returns the string representation of card (s,r)*)
let string_of_card (s,r) : string =
  (rank_to_string r) ^ " of " ^ (suit_to_string s)

(*
(* Finished and tested 11/26 - Vanya *)
(* play_card p c removes card c from the hand of player p and returns a new
 * player object. Raises Invalid_action if the card is not in p's hand*)
let play_card (p : player) (c : card) : player =
  let new_hand = List.filter (fun x -> x <> c) p.hand in
  if List.length new_hand = List.length p.hand then
    raise (Invalid_action ("Card is not in "^p.name^"'s hand"))
  else
    {p with hand = new_hand}


(* returns a new gamestate in which the active player has played card c *)
let game_play_card (g : state) (c : card) : state =
  let p = play_card g.active c in
  let replace = fun x -> if x = g.active then p else x in
  let attackers' = List.map replace g.attackers in
  let defender' = if g.defender = g.active then p else g.defender in
  { deck = g.deck;
   trump = g.trump;
   attackers = attackers';
   defender = defender';
   table = g.table;
   active = g.active;
   discard = g.discard;
   winners = g.winners;
  }


(* returns true iff d is a valid defense to attack a *)
let valid_defense (a : card) (d : card) : bool =
  failwith "unimplemented"

(* returns a new gamestate in which all attackers have been dealt cards
 * from the deck until everyone has 6 cards or the deck in empty *)
let deal (g : state) : state =
  failwith "unimplemented"

(* returns the last element of a list. Raises Invalid_action if the list is empty*)
let rec last_attacker = function
  | [] -> raise (Invalid_action "the game is over")
  | h::[] -> h
  | h::t -> last_attacker t


(* removes the last element of a list.  Leaves the empty list unchanged *)
let rec remove_last = function
  | [] -> []
  | _::[] -> []
  | h::t -> h::(remove_last t)



(* returns a new state in which d is the defender, and the attackers have
 * been changed accordingly *)
let new_turn g (d : player) : state =
  let a = g.defender::(remove_last g.attackers) in
  let a' = if d = (last_attacker g.attackers)
    then a
    else (last_attacker g.attackers)::(remove_last a)
    in
  { deck = g.deck;
   trump = g.trump;
   attackers = a';
   defender = d;
   table = g.table;
   active = g.active;
   discard = g.discard;
   winners = g.winners;
  }

(* returns the player who comes after the active player in the attacker list.
 * raise Invalid_action if the next player should be the defender *)
let next_attacker (g : state) : player =
  failwith "unimplemented"

(* returns a new gamestate with attack c added to the table *)
let add_attack g (c : card) : state =
  { deck = g.deck;
   trump = g.trump;
   attackers = g.attackers;
   defender = g.defender;
   table = (c,None)::g.table;
   active = g.active;
   discard = g.discard;
   winners = g.winners;
  }


(* returns true iff all the attacks on the table have rank r and have no
 * defending card *)
let rec deflectable (r : int) = function
  | [] -> true
  | ((_,r'),None)::t -> r = r' && deflectable r t
  | _ -> false


(* changes active player to p. Does not change any other field *)
let rec change_active g (p : player) =
  { deck = g.deck;
   trump = g.trump;
   attackers = g.attackers;
   defender = g.defender;
   table = g.table;
   active = p;
   discard = g.discard;
   winners = g.winners;
  }


(* Terminates everything and prints either a positive or negative message
 * depending on whether the player won or is a Durak. *)
let end_game (g : state) : state =
  failwith "unimplemented"


(* makes player p a winner. Prereq: p must be an attacker *)
let do_win (g : state) (p : player) : state =
  if List.length g.attackers = 1 then end_game g else
  { deck = g.deck;
   trump = g.trump;
   attackers = List.filter (fun x -> x<>p) g.attackers;
   defender = g.defender;
   table = g.table;
   active = g.active;
   discard = g.discard;
   winners = p::(g.winners);
  }



(* carry out the command "deflect against c1 with c2"
 * raise Invalid_action if appropriate
 * returns the new gamestate and {active player won} *)
let deflect (g : state) (s1,r1) (s2,r2) : state*bool =
  if r1 <> r2
  then raise (Invalid_action "You must deflect with a card of the same rank")
  else match g.table with
  | [] -> raise (Invalid_action "There is no attack to deflect!")
  | l -> begin
    if deflectable r1 l
    then
      let g''' = game_play_card g (s2,r2) in
      let won = g'''.active.hand = [] in
      let active = g'''.active in
      let next_p = last_attacker g'''.attackers in
      let g' = new_turn g''' next_p in
      let g'' = add_attack g' (s2,r2) in
      let g'''' = if won then do_win g'' active else g'' in
      (change_active g'''' next_p,won)
    else raise (Invalid_action "Can't deflect")
  end
*)


(* Draws and prompts the user*)
let game_draw (g : state) (prompt : string) : string =
  Gui.draw g;
  print_endline prompt;
  read_line ()


(* Prompts the user for a response until the user types a valid input.
 * p0 is the original prompt.  p1 is the prompt with additional feedback. *)
let rec parse_no_fail' (p0 : string) (p1 : string) g : command =
  let r = game_draw g p1 in
  try parse r with
  | Cannot_parse _ ->
      let m = "I couldn't understand \"" ^ r ^ "\".\nPlease try again\n" ^ p0 in
      parse_no_fail' p0 m g


(* Prompts the user for a response until the user types a valid input
 * unless the active player is an AI, in which case an AI response will be
 * requested *)
let parse_no_fail (p : string) (g : state) : command =
  if g.active.state = Human then parse_no_fail' p p g else Ai.response g



(*
(* returns a new gamestate in which all cards on the table have been added
 * to the hand of the active player *)
let take_all (g : state) : state =
  failwith "unimplemented"


(* if c1 is on the table, and if c2 is a valid defense, return a new gamestate
 * with c2 paired to c1 on the table.  Raise Invalid_action otherwise. *)
let place_defense (g : state) (c1 : card) (c2 : card) : state =
  (* implementation tip: Use valid_defense as a helper *)
  failwith "unimplemented"


(* returns true iff the table is empty or at least one of the cards on the table
 * has the same rank as c *)
let valid_attack (g : state) (c : card) : bool =
  failwith "unimplemented"


(* loops over attackers if everyone has been passing until someone doesn't pass*)
let rec pass' (g : state) : state*command =
  let g' = change_active g (next_attacker g) in
    let prompt = g.active.name ^ " passed. You can pass or attack." in
    let response = parse_no_fail prompt g' in
    let first = g.active = (List.hd g.attackers) in
    if response <> Pass || not first then (g', response) else
    if g'.active = last_attacker g'.attackers
      then let g'' = change_active g g.defender in
      let prompt = "Everyone passed. You can take or defend." in
      let response = parse_no_fail prompt g'' in
      (g'',response)
      (*TODO: Somehow make sure no one can attack anymore*)
    else pass' g'
*)

(* Calls itself recursively to update the state in response to commands *)
let rec repl g c = step g c
(*
  | Attack c -> failwith "unimplemented"
  | Defend (c1,c2) -> failwith "unimplemented"
  | Take -> failwith "unimplemented"
  | Pass -> pass g
  | Deflect (c1,c2) -> begin
      let (g',won) = deflect g c1 c2 in
      if g'.active.state = Human
      then
        let prev_player = g.active.name in
        let prompt = prev_player ^ " deflected. You can deflect, take, or defend." in
        let prompt' = if won then prev_player ^ " is out of the game. " ^ prompt
                    else prompt in
        let c' = parse_no_fail prompt' g in repl g' c'
  end


(* changes state as necessary for pass.  Keeps track of who has passed
 * until one attacker doesn't pass or until all attackers have passed *)
and pass (g : state) : unit =
  if g.active = g.defender then raise (Invalid_action "Only attackers can pass.") else
  if g.active = last_attacker g.attackers
    then let g' = change_active g g.defender in
      let prompt = g.active.name ^ " passed. You can take or defend." in
      let response = parse_no_fail prompt g' in
      repl g' response else
  let g',c = pass' g in repl g' c
*)

(*TEST CASES*)

let test_string_of_card () =
  let cards = [(Heart, 6); (Diamond, 7); (Spade, 8); (Club,9); (Spade, 10);
               (Heart, 11);(Diamond, 12);(Spade, 13);(Club,14)] in
  assert (string_of_card (List.nth cards 0) = "6 of Hearts");
  assert (string_of_card (List.nth cards 1) = "7 of Diamonds");
  assert (string_of_card (List.nth cards 2) = "8 of Spades");
  assert (string_of_card (List.nth cards 3) = "9 of Clubs");
  assert (string_of_card (List.nth cards 4) = "10 of Spades");
  assert (string_of_card (List.nth cards 5) = "Jack of Hearts");
  assert (string_of_card (List.nth cards 6) = "Queen of Diamonds");
  assert (string_of_card (List.nth cards 7) = "King of Spades");
  assert (string_of_card (List.nth cards 8) = "Ace of Clubs")

let test_play_card () =
  ()
(*
  let hand1 = [(Heart, 7); (Diamond, 7);  (Club,14); (Spade, 14)] in
  let player1 = {state = Human; hand = hand1; name = "Zapdoz"} in
  let test_hand1 = [(Diamond, 7);  (Club,14); (Spade, 14)] in
  let test_hand2 = [(Heart, 7);  (Club,14); (Spade, 14)] in

  assert (play_card player1 (Heart,7) = {player1 with hand = test_hand1});
  assert (play_card player1 (Diamond, 7) = {player1 with hand = test_hand2});
  assert (let p1 = play_card player1 (Club,14) in
          let p2 = play_card p1 (Spade, 14) in
          let p3 = play_card p2 (Diamond, 7) in
          let p4 = play_card p3 (Heart, 7) in
          p4 = {player1 with hand = []})
  (* Need test case for error, but I think it works. -Vanya*)
*)

let test_game_play_card () =
  ()

let test_next_attacker () =
  ()

let test_valid_defense () =
  ()

let test_deal () =
  ()

let test_last_attacker () =
  ()

let test_remove_last () =
  ()

let test_new_turn () =
  ()

let test_add_attack () =
  ()

let test_deflectable () =
  ()

let test_change_active () =
  ()

let test_do_win () =
  ()

let test_deflect () =
  ()

let test_parse_no_fail () =
  ()

let test_take_all () =
  ()

let test_place_defense () =
  ()

let test_valid_defense () =
  ()

let test_pass' () =
  ()

let test_pass () =
  ()

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
(*
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
*)
  ()

let test_init_game_state () =
(*
  let g1 = init_game_state "jane" 1 2 3 in
  assert (List.length (g1.deck) = 12);
  assert ((g1.defender).state = CPU 1);
  assert (List.length ((g1.defender).hand) = 6);
  assert ((g1.active).state = Human);
  assert ((g1.active).name = "jane");
  assert (List.length ((g1.active).hand) = 6);
*)
  ()

let run_tests () =
  test_string_of_card ();
  test_play_card ();
  test_split ();
  test_parse_rank ();
  test_parse_suit ();
  test_parse_card ();
  test_parse ();
(*   test_init_deck (); *)
  test_init_game_state ();
  print_endline "all tests pass";
  ()

let _ =
  title_screen ();
  run_tests ()
