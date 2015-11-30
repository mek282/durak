open Cards

exception Invalid_action of string

(* ========================================================================== *)
(* ==============================INITIALIZER================================= *)
(* ========================================================================== *)

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
 * out
 * Prec: dlist is a list of at least one int. *)
let init_game_state s dlist =
  let start_deck = init_deck () in
  let h1 = deal_hand (snd start_deck) in
  let p1 = {state = Human; hand = (fst h1); name = s} in
  let rec create_bots deck ds plist = (
    match ds with
    | [] -> (plist, deck)
    | hd::tl -> let deal = deal_hand deck in
                let p = {state = CPU hd; hand = (fst deal); name = (gen_name hd)} in
                create_bots (snd deal) tl (plist @ [p]) ) in
  let deal_all = create_bots (snd h1) dlist [] in
  { deck = (snd deal_all);
    trump = (fst start_deck);
    attackers = p1 :: (List.tl (fst deal_all));
    defender = List.hd (fst deal_all);
    table = [];
    active = p1;
    discard = [];
    winners = [];
  }

(* ========================================================================== *)
(* =========================COMMAND PROCESSING=============================== *)
(* ========================================================================== *)

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
  { g with attackers = attackers'; defender = defender'}


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
  { g with attackers = a'; defender = d}

(* returns the player who comes after the active player in the attacker list.
 * raise Invalid_action if the next player should be the defender *)
let next_attacker (g : state) : player =
  failwith "unimplemented"

(* returns a new gamestate with attack c added to the table *)
let add_attack g (c : card) : state =
  { g with table = (c,None)::g.table}


(* returns true iff all the attacks on the table have rank r and have no
 * defending card *)
let rec deflectable (r : int) = function
  | [] -> true
  | ((_,r'),None)::t -> r = r' && deflectable r t
  | _ -> false


(* changes active player to p. Does not change any other field *)
let rec change_active g (p : player) =
  {g with active = p}


(* Terminates everything and prints either a positive or negative message
 * depending on whether the player won or is a Durak. *)
let end_game (g : state) : state =
  failwith "unimplemented"


(* makes player p a winner. Prereq: p must be an attacker *)
let do_win (g : state) (p : player) : state =
  if List.length g.attackers = 1 then end_game g else
  { g with
   attackers = List.filter (fun x -> x<>p) g.attackers;
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

(*
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

(* If the active player is an attacker holding card c, and card c is a valid
 * attack, remove that card from the active player's hand and add that card
 * to the table, then change the active player to either the next attacker
 * or to the defender, if the active player is the last attacker *)
let attack (g : state) (c : card) =
  if not (List.mem g.active g.attackers)
    then raise (Invalid_action "You are not an attacker!")
  else if not (valid_attack g c)
    then raise (Invalid_action "That is not a valid attack!")
  else let g' = game_play_card g c in
  let table' = (c,None)::g.table in
  let active' =
    if g.active = last_attacker g.attackers
      then g.defender
      else next_attacker g in
  {g' with table=table'; active=active'}


let step (g:state) (c:command) : state =
  match c with
  | Attack c -> (try attack g c with Invalid_action a -> print_endline a; g)
  | Defend (c1,c2) -> failwith "unimplemented"
  | Take -> failwith "unimplemented"
  | Pass -> failwith "unimplemented"
  | Deflect (c1,c2) -> failwith "unimplemented"


(* ========================================================================== *)
(* ==============================TESTING===================================== *)
(* ========================================================================== *)

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
  let g1 = init_game_state "jane" [1;2;3] in
  assert (List.length (g1.deck) = 12);
  assert ((g1.defender).state = CPU 1);
  assert (List.length ((g1.defender).hand) = 6);
  assert ((g1.active).state = Human);
  assert ((g1.active).name = "jane");
  assert (List.length ((g1.active).hand) = 6)

let test_play_card () =
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

let test_game_play_card () =
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

let test_next_attacker () =
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

let test_take_all () =
  ()

let test_place_defense () =
  ()

let test_pass' () =
  ()

let test_pass () =
  ()

let run_tests () =
  test_init_deck ();
  test_init_game_state ();
  test_play_card ();
  print_endline "all tests pass";
  ()

let _ = run_tests ()