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
let valid_defense (a : card) (d : card) (trump: suit): bool =
  if (fst a) = (fst d) then (snd a) < (snd d)
  else if (fst d) = trump then true
  else false

(* returns a player list with each hand having at least six cards (until deck is
 * empty) and a deck with those cards removed. *)
let rec deal_helper (plist: player list) (deck: deck): player list * deck =
  match plist with
  | [] -> (plist, deck)
  | h::t -> let len = List.length h.hand in
            if len < 6 || List.length deck = 0 then
              let (card,newdeck) = match deck with
                                   | [] -> ([], [])
                                   | h::t -> ([h], t)
              in
              let newplayer = {h with hand = card @ h.hand} in
              deal_helper (newplayer :: t) newdeck
            else
              let (newplist, newdeck) = deal_helper t deck in
              (h :: newplist, newdeck)

(* returns a new gamestate in which all attackers have been dealt cards
 * from the deck until everyone has 6 cards or the deck in empty *)
let deal (g : state) : state =
  let players = g.attackers @ [g.defender] in
  let (newplayers, newdeck) = deal_helper players g.deck in
  match List.rev newplayers with
  | []      -> raise (Invalid_action "game should be over, error in deal")
  | h :: [] -> raise (Invalid_action "game should be over, error in deal")
  | h :: t  -> {g with defender  = h;
                       attackers = t;
                       deck      = newdeck}


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


(* [before el lst] returns the element in lst that comes before el.
 * Precondition: There must be at least two elements in the list, and el
 * cannot be the first element *)
let rec before el = function
  | [] -> failwith "crashed on before"
  | h::[] -> raise (Invalid_action "The next player is the defender")
  | e1::e2::t -> if e2 = el then e1 else before el (e2::t)


(* returns the player who comes after the active player in the attacker list.
 * raise Invalid_action if the next player should be the defender
 * Raises Invalid_action if the active player is not an attacker *)
let next_attacker (g : state) : player =
  if not (List.mem g.active g.attackers)
    then raise (Invalid_action "The current player isn't an attacker!") else
  before g.active g.attackers


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
  let _ =
    if List.exists (fun x -> x.state = Human) g.winners
    then print_endline "Congratualtions! You didn't lose!"
    else print_endline "You lost--You're the durak!" in
  exit 0


(* makes player p a winner. Ends the game if only one player is left. *)
let do_win (g : state) (p : player) : state =
  if List.length g.attackers = 1 then end_game g else
  if g.defender = p
    then let g' = new_turn g (last_attacker g.attackers) in
    { g' with winners = p::(g'.winners) }
  else
  let active' = if g.active = p
      then if p = last_attacker g.attackers then g.defender else next_attacker g
    else g.active in
  { g with
   active = active';
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


(* Converts a list of type ('a,'a Option) to a list of type 'a with each
 * non-None member of a pair made into its own element *)
let rec tablepairs_to_list = function
  | [] -> []
  | (c1,Some c2)::t -> c1::c2::(tablepairs_to_list t)
  | (c1, None)::t -> c1::(tablepairs_to_list t)


(* returns a new gamestate in which all cards on the table have been added
 * to the hand of the active player
 * Precondition: the active player must be the defender *)
let take_all (g : state) : state =
  if g.active <> g.defender
    then raise (Invalid_action "You're not the defender!") else
  let table = tablepairs_to_list g.table in
  let p = {g.active with hand = table@g.active.hand} in
  { g with active = p; defender = p; table = [] }


(* if c1 is on the table, and if c2 is a valid defense, return a new gamestate
 * with c2 paired to c1 on the table and with c2 removed from the active
 * player's hand.
 * Raise Invalid_action otherwise. *)
let place_defense (g : state) (c1 : card) (c2 : card) : state =
  (* implementation tip: Use valid_defense as a helper *)
  failwith "unimplemented"


(* returns true iff the table is empty or at least one of the cards on the table
 * has the same rank as c *)
let valid_attack (g : state) ((s : suit) , (r : int)) : bool =
  g.table = [] || (List.exists (fun (_,x) -> x = r) (tablepairs_to_list g.table))

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
let attack (g : state) (c : card) : state*bool =
  if not (List.mem g.active g.attackers)
    then raise (Invalid_action "You are not an attacker!")
  else if not (valid_attack g c)
    then raise (Invalid_action "That is not a valid attack!")
  else let g' = game_play_card g c in
  let won = g'.active.hand = [] in
  let g'' = if won then do_win g' g'.active else g' in
  let table' = (c,None)::g''.table in
  let active' =
    if g''.active = last_attacker g''.attackers
      then g''.defender
      else next_attacker g'' in
  ( {g'' with table=table'; active=active'}, won)


(* returns the penultimate element of a list, unless the list only has one
 * element, in which case that element is returned. Raises Invalid_action for
 * the empty list. *)
let rec penultimate = function
  | [] -> raise (Invalid_action "The list is empty")
  | h::[] -> h
  | h::_::[] -> h
  | h::t -> penultimate t

(* [all_answered g] returns true iff (c,None) is not a member of g.table
 * for any c. In other words, every card on the table is paired with some
 * other card. *)
let all_answered (g : state) : bool =
  not (List.exists (fun (_,x) -> x = None) g.table)


(* Carries out "defende against c1 with c2" --
 * if the active player is the defender, c1 is an unanswered attack on the
 * table, c2 is a valid defense to c1, and c2 is in the active player's hand,
 * then [defend g c1 c2] removes card c2 from the active player's hand and
 * adds it to the table as a response to c1; otherwise Invalid_action is raised.
 * If all attacks on the table have been answered, the active player becomes
 * the attacker; otherwise the defender gets a chance to play again.
 * Makes defender a winner if c2 was her last card.
 * Also returns a bool telling whether or not g.active won *)
let defend (g : state) (c1 : card) (c2 : card) : state*bool =
  if g.active <> g.defender
    then raise (Invalid_action "Only the defender can defend.") else
  if not (List.mem (c1,None) g.table)
    then raise (Invalid_action (string_of_card c1 ^ " is not on the table!")) else
  let g' = place_defense g c1 c2 in
  let won = g'.active.hand = [] in
  if won then ({ (do_win g' g'.active) with table = [] },won) else
  if all_answered g'
    then let g'' = {g' with table = []} in
      (new_turn g'' (last_attacker g''.attackers) , won)
    else (g', won)


(* makes the next player the active player *)
let pass (g : state) : state =
  let active' =
    if g.active = g.defender
      then last_attacker g.attackers
      else try next_attacker g with Invalid_action _ -> g.defender
  in { g with active = active' }


(* returns the state that would result from applying c to g, as well as
 * a string describing what was done *)
let step (g:state) (c:command) : state*string =
  match c with
  | Attack c -> begin
      try
        let (g',w) = attack g c in
        let win_m = if w then g.active.name ^ " won! " else "" in
        let m=g.active.name^" attacked with "^string_of_card c^". " ^ win_m in
        (g',m)
      with
      | Invalid_action a -> (g, "There was a problem: " ^ a)
  end
  | Defend (c1,c2) -> begin
      try
        let (g',w) = defend g c1 c2 in
        let win_m = if w then g.active.name ^ " won! " else "" in
        let m=g.active.name^" defended with "^string_of_card c2^". " ^ win_m in
        (g',m)
      with
      | Invalid_action a -> (g, a)
  end
  | Take -> begin
    try
      let g' = take_all g in
      let g'' = new_turn g' (penultimate g.attackers) in
      let m = g.active.name ^ " chose to take." in
      (g'',m)
    with
    | Invalid_action a -> (g, a)
  end
  | Pass -> let m = g.active.name ^ "passed." in (pass g, m)
  | Deflect (c1,c2) -> begin
    try
      let (g',w) = deflect g c1 c2 in
      let win_m = if w then g.active.name ^ " won! " else "" in
      let m=g.active.name^" deflected with "^string_of_card c2^". " ^ win_m in
      (g',m)
    with
    | Invalid_action a -> (g,a)
  end


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
  let trump = Heart in
  assert (valid_defense (Spade, 6) (Spade,7) trump);
  assert (valid_defense (Diamond, 11) (Diamond,14) trump);
  assert (valid_defense (Diamond, 13) (Heart,7) trump);
  assert (valid_defense (Club, 10) (Heart,10) trump);
  assert (valid_defense (Heart, 10) (Heart,11) trump);
  assert (valid_defense (Club, 8) (Club, 12) trump);

  assert (not (valid_defense (Club, 12) (Club, 8) trump));
  assert (not (valid_defense (Diamond, 7) (Club, 8) trump));
  assert (not (valid_defense (Heart, 8) (Spade, 12) trump));
  assert (not (valid_defense (Club, 6) (Spade, 6) trump));
  assert (not (valid_defense (Heart, 14) (Heart, 10) trump));
  assert (not (valid_defense (Club, 11) (Club, 9) trump))

(* This test works, but need to also check for when deck becomes empty
 * but I need sleep now. *)
let test_deal () =
  let deck = [(Heart, 9); (Diamond, 9);  (Club, 9); (Spade, 9)] in
  let trump = Heart in

  let hand1 = [(Heart, 7); (Diamond, 7);  (Club,14); (Spade, 14)] in
  let player1 = {state = Human; hand = hand1; name = "Zapdoz"} in

  let hand2 = [(Diamond, 6); (Club, 10); (Club, 12); (Spade,13); (Diamond, 14)] in
  let player2 = {state= CPU(1); hand = hand2; name = "Rawr"} in

  let attackers = [player2] in
  let defender = player1 in
  let table = [((Club, 6), Some(Club,7));
               ((Diamond,6), Some(Diamond,10));
               ((Club,8), None);
               ((Heart,8), None);
               ((Spade, 8), Some(Spade,9))] in

  let active = player1 in
  let discard = [] in
  let winners = [] in
  let state = {deck=deck; trump=trump; attackers=attackers; defender=defender;
               table=table; active=active; discard=discard; winners=winners} in
  let dealt_state = deal state in
  let attacker1 = List.nth dealt_state.attackers 0 in

  assert (dealt_state.deck          = [(Spade, 9)]);
  assert (dealt_state.defender.hand = [(Club, 9); (Diamond, 9); (Heart, 7);
                                      (Diamond, 7); (Club,14); (Spade, 14)]);
  assert (attacker1.hand            = [(Heart, 9); (Diamond, 6); (Club, 10);
                                      (Club, 12); (Spade,13); (Diamond, 14)])

let test_last_attacker () =
  let hand1 = [(Heart, 7); (Diamond, 7);  (Club,14); (Spade, 14)] in
  let player1 = {state = Human; hand = hand1; name = "Zapdoz"} in
  let hand2 = [(Diamond, 6); (Club, 10); (Club, 12); (Spade,13); (Diamond, 14)] in
  let player2 = {state= CPU(1); hand = hand2; name = "Rawr"} in

  let lst = [1;2;3;4] in
  let lst1 = [1] in
  let lst2 = [player1; player2] in
  assert (last_attacker lst = 4);
  assert (last_attacker lst1 = 1);
  assert (last_attacker lst2 = player2)

let test_remove_last () =
  let hand1 = [(Heart, 7); (Diamond, 7);  (Club,14); (Spade, 14)] in
  let player1 = {state = Human; hand = hand1; name = "Zapdoz"} in
  let hand2 = [(Diamond, 6); (Club, 10); (Club, 12); (Spade,13); (Diamond, 14)] in
  let player2 = {state= CPU(1); hand = hand2; name = "Rawr"} in

  let lst = [1;2;3;4] in
  let lst1 = [1] in
  let lst2 = [player1; player2] in
  assert (remove_last lst = [1;2;3]);
  assert (remove_last lst1 = []);
  assert (remove_last lst2 = [player1])

let test_new_turn () =
  let deck = [(Heart, 9); (Diamond, 9);  (Club, 9); (Spade, 9)] in
  let trump = Heart in

  let hand1 = [(Heart, 7); (Diamond, 7);  (Club,14); (Spade, 14)] in
  let player1 = {state = Human; hand = hand1; name = "Zapdoz"} in

  let hand2 = [(Diamond, 6); (Club, 10); (Club, 12); (Spade,13); (Diamond, 14)] in
  let player2 = {state= CPU(1); hand = hand2; name = "Rawr"} in

  let hand3 = [(Diamond, 11); (Club, 11); (Club, 9)] in
  let player3 = {state= CPU(1); hand = hand3; name = "Ayyy"} in

  let hand4 = [(Club, 7); (Club, 8); (Heart, 10); (Spade,10)] in
  let player4 = {state= CPU(1); hand = hand4; name = "Lmao"} in

  let attackers = [player2; player3; player4] in
  let defender = player1 in
  let table = [((Club, 6), Some(Club,7));
               ((Diamond,6), Some(Diamond,10));
               ((Club,8), None);
               ((Heart,8), None);
               ((Spade, 8), Some(Spade,9))] in

  let active = player1 in
  let discard = [] in
  let winners = [] in
  let state = {deck=deck; trump=trump; attackers=attackers; defender=defender;
               table=table; active=active; discard=discard; winners=winners} in
  let new_state = new_turn state player4 in

  assert (new_state.defender  = player4);
  assert (new_state.attackers = [player1; player2; player3])

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
  test_string_of_card ();
  test_init_deck ();
  test_init_game_state ();
  test_play_card ();
  test_valid_defense ();
  test_deal ();
  test_last_attacker ();
  test_remove_last ();
  test_new_turn ();
  print_endline "all tests pass";
  ()

let _ = run_tests ()