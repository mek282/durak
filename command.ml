open Cards

exception Invalid_action of string


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

let step (g:state) (c:command) : state =
  match c with
  | Attack c -> failwith "unimplemented"
  | Defend (c1,c2) -> failwith "unimplemented"
  | Take -> failwith "unimplemented"
  | Pass -> failwith "unimplemented"
  | Deflect (c1,c2) -> failwith "unimplemented"
