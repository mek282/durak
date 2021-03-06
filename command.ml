open Cards

exception Invalid_action of string

(* ========================================================================== *)
(* ==============================INITIALIZER================================= *)
(* ========================================================================== *)

(* Helper for init_deck, [unshuffled_deck [] 0] will create an unshuffled deck of
 * standard cards > 6. *)
let rec unshuffled_deck d n =
  if n < 15
  then let new_d = List.append d [(Heart, n); (Diamond, n);
                                 (Spade, n); (Club, n)] in
        unshuffled_deck new_d (n+1)
  else d

(* Helper for init_deck to randomly cut a deck: input [shuffle deck [] []]. *)
let rec shuffle l1 l2 l3 =
  match l1 with
  | e1::e2::tl -> shuffle tl (e1::l2) (e2::l3)
  | e1::tl -> shuffle tl l2 (e1::l3)
  | [] -> (l2, l3)

(* Helper for init_deck to choose the trump card. *)
let choose_trump () =
  let n = Random.int 4 in
  if n = 0 then Heart
    else if n = 1 then Diamond
    else if n = 2 then Spade
  else Club

(* Creates a randomized 36 card deck in which each card is different. The deck
 * is a standard deck with twos, threes, fours, and fives removed.
 * [init_deck ()] outputs a pair representing (trump, deck). *)
let init_deck _ : suit * deck =
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

(* [deal_hand d] will output a hand of length 6 along with the rest of the
 * deck in the form [(hand, deck)]. Used to initialize game state, NOT to
 * update hands. *)
let deal_hand d =
  match d with
  | c1::c2::c3::c4::c5::c6::tl -> ([c1; c2; c3; c4; c5; c6], tl)
  | _ -> failwith "The deck is too small - only 4 players max!"

let e_names = ref ["Mr. Camel"; "Camille"; "Cameron"; "Melly"; "Camelot"]
let m_names = ref ["Drew"; "Ivan"; "Jose"; "Mary"; "Ivan II"]
let h_names = ref ["Clarkson"; "Chirag"; "Remy"; "M. George"; "Amber"]

(* Randomly generates bot names, ensuring the human player's name does not
 * match. [gen_name d s] chooses a name for a bot of difficulty d where
 * s is the player's name. *)
let rec gen_name d s =
  if d = 1
    then let n = List.nth (!e_names) (Random.int (List.length (!e_names))) in
         e_names := List.filter (fun x -> x <> n) (!e_names);
         if n <> s then n else gen_name d s
  else if d = 2
    then let n = List.nth (!m_names) (Random.int (List.length (!m_names))) in
         m_names := List.filter (fun x -> x <> n) (!m_names);
         if n <> s then n else gen_name d s
  else let n = List.nth (!h_names) (Random.int (List.length (!h_names))) in
         h_names := List.filter (fun x -> x <> n) (!h_names);
         if n <> s then n else gen_name d s

(* Creates an initial state for the game in which all cards have been passed
 * out. [init_game_state s dlist] outputs a game state with a human player
 * named s and bots of all the difficulties in dlist.
 * Precondition: dlist is a list of at least one int. *)
let init_game_state s dlist =
  let start_deck = init_deck () in
  let h1 = deal_hand (snd start_deck) in
  let p1 = {state = Human; hand = (fst h1); name = s} in
  let rec create_bots deck ds plist = (
    match ds with
    | [] -> (plist, deck)
    | hd::tl -> let deal = deal_hand deck in
                let p = {state = CPU hd; hand = (fst deal); name = (gen_name hd s)} in
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
    passed = [];
  }


(* ========================================================================== *)
(* =========================STRING & PRINT FUNS============================== *)
(* ========================================================================== *)

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

(* returns the string representation of card (s,r)*)
let string_of_card (s,r) : string =
  (rank_to_string r) ^ " of " ^ (suit_to_string s)

(* returns the string representation of a deck *)
let rec string_of_deck = function
  | [] -> ""
  | h::t -> (string_of_card h) ^ "; " ^ (string_of_deck t)

(* returns string representation of a player *)
let string_of_player (p : player) =
  let s = match p.state with
          | Human -> "Human"
          | CPU i -> "CPU" ^ (string_of_int i) in
  let hand = string_of_deck p.hand in
  "(" ^ p.name ^ ": " ^ s ^ ", " ^ hand ^ ")"

(* returns string representation of a list of players *)
let rec string_of_attackers = function
  | [] -> ""
  | h::t -> (string_of_player h) ^ "; " ^ (string_of_attackers t)

(* returns string representation of table *)
let rec string_of_table = function
  | [] -> ""
  | (a,Some d)::t ->
    "(" ^ string_of_card a ^ ", " ^ string_of_card d ^ ") " ^ string_of_table t
  | (a,None)::t ->
    "(" ^ string_of_card a ^ ", None) " ^ string_of_table t

(* returns string representation of game state *)
let print_state (g : state) : unit =
  print_endline("deck: " ^ string_of_deck g.deck);
  print_endline("trump: " ^ suit_to_string g.trump);
  print_endline("attackers: " ^ string_of_attackers g.attackers);
  print_endline("defender: " ^ string_of_player g.defender);
  print_endline("table: " ^ string_of_table g.table);
  print_endline("active: " ^ string_of_player g.active);
  print_endline("discard: " ^ string_of_deck g.discard);
  print_endline("winners: " ^ string_of_attackers g.winners);
  print_endline("passed: " ^ string_of_attackers g.passed)


(* ========================================================================== *)
(* =========================COMMAND PROCESSING=============================== *)
(* ========================================================================== *)


(* Converts a list of type ('a,'a Option) to a list of type 'a with each
 * non-None member of a pair made into its own element *)
let rec tablepairs_to_list = function
  | [] -> []
  | (c1,Some c2)::t -> c1::c2::(tablepairs_to_list t)
  | (c1, None)::t -> c1::(tablepairs_to_list t)


(* play_card p c removes card c from the hand of player p and returns a new
 * player object. Raises Invalid_action if the card is not in p's hand*)
let play_card (p : player) (c : card) : player =
  let new_hand = List.filter (fun x -> x <> c) p.hand in
  if List.length new_hand = List.length p.hand then
    raise (Invalid_action ("Card is not in " ^ p.name ^ "'s hand"))
  else {p with hand = new_hand}


(* returns a new gamestate in which the active player has played card c *)
let game_play_card (g : state) (c : card) : state =
  let p = play_card g.active c in
  let replace = fun x -> if x.name = g.active.name then p else x in
  let attackers' = List.map replace g.attackers in
  let defender' = if g.defender.name = g.active.name then p
  else g.defender
  in { g with attackers = attackers'; defender = defender'; active = p}


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
            if len < 6 && not (List.length deck = 0) then
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
                       attackers = List.rev t;
                       deck      = newdeck}


(* Returns the last element of a list. Raises Invalid_action if the list is [] *)
let rec last_attacker = function
  | [] -> raise (Invalid_action "the game is over")
  | h::[] -> h
  | h::t -> last_attacker t


(* Removes the last element of a list. Leaves the empty list unchanged *)
let rec remove_last = function
  | [] -> []
  | _::[] -> []
  | h::t -> h::(remove_last t)


(* returns a new state in which d is the defender, and the attackers have
 * been changed accordingly.  Attackers from the previous round are dealt
 * cards from the deck if need be. The primary attacker is set to be the
 * active player.  The table is cleared. *)
let new_turn g (d : player) : state =
  let g' = deal g in
  let a = g'.defender::(remove_last g'.attackers) in
  let a' = if d.name = (last_attacker g'.attackers).name
    then a
    else (last_attacker g'.attackers)::(remove_last a)
    in
  let d' = List.find (fun x -> x.name = d.name) (g'.defender :: g'.attackers) in
  let g'' = { g' with attackers = a'; defender = d'; passed = [] } in
  { g'' with active = List.hd g''.attackers;
             table = [];
             discard = (tablepairs_to_list g.table) @ g.discard }


(* [before el lst] returns the element in lst that comes before el.
 * Precondition: There must be at least two elements in the list, and el
 * cannot be the first element *)
let rec before (el: 'a) (plist: 'a list) =
  match plist with
  | [] -> failwith "crashed on before"
  | h::[] -> raise (Invalid_action "The next player is the defender")
  | e1::e2::t -> if e2.name = el.name then e1 else before el (e2::t)


(* returns the player who comes after the active player in the attacker list.
 * raise Invalid_action if the next player should be the defender
 * Raises Invalid_action if the active player is not an attacker *)
let next_attacker (g : state) : player =
  if not (List.exists (fun x -> x.name = g.active.name) g.attackers)
    then raise (Invalid_action "The current player isn't an attacker!")
  else before g.active g.attackers


(* returns a new gamestate with attack c added to the table *)
let add_attack g (c : card) : state =
  { g with table = (c,None)::g.table}


(* returns true iff all the attacks on the table have rank r and have no
 * defending card *)
let rec deflectable (r : int) = function
  | [] -> false
  | ((_,r'), None)::[] -> r = r'
  | ((_,r') ,None)::t -> r = r' && deflectable r t
  | _ -> false


(* changes active player to p. Does not change any other field *)
let rec change_active g (p : player) =
  {g with active = p}


(* makes player p a winner. Ends the game if only one player is left.
 * changes active player, attackers, and defender as necessary so that p is no
 * longer represented in state except as a winner.
 * Returns the gamestate in which p is a winner and [the game has ended] *)
let do_win (g : state) : state*bool =
  if (List.length g.attackers = 1) && g.active.name = g.defender.name then
    ({ g with attackers = [];
              active = last_attacker g.attackers;
              winners = g.active::g.winners}, true)
  else
    if g.defender.name = g.active.name then
      let g' = new_turn g (last_attacker g.attackers) in
      ({ g' with winners = g.active::(g'.winners);
                 attackers = List.tl g'.attackers;
                 active = List.hd (List.tl g'.attackers)}, false)
    else
      if List.length g.attackers = 1 then
        ({ g with attackers = [];
                  active = g.defender;
                  winners = g.active::g.winners},true)
      else
        let active' = if g.active.name = (List.hd g.attackers).name then
                        g.defender
                      else next_attacker g in
        ({ g with
         active = active';
         attackers = List.filter (fun x -> x.name <> g.active.name) g.attackers;
         winners = g.active::(g.winners);}, false)

(* carry out the command "deflect against c1 with c2"
 * raise Invalid_action if appropriate
 * returns the new state and {active player won} and {game is over}*)
let deflect (g : state) (s1,r1) (s2,r2) : state*bool*bool =
  if r1 <> r2
  then raise (Invalid_action "You must deflect with a card of the same rank")
  else match g.table with
  | [] -> raise (Invalid_action "There is no attack to deflect!")
  | l -> begin
    if deflectable r1 l
    then
      let g''' = game_play_card g (s2,r2) in
      let won = g'''.active.hand = [] in
      if won then let (gfinal,ended) = do_win g''' in (gfinal,won,ended)
      else
        let next_p = if List.length g.attackers = 1
                  then g'''.defender else  last_attacker g'''.attackers in
        let g' = new_turn g''' next_p in
        let g'' = add_attack g' (s2,r2) in
        ({(change_active g'' next_p) with table = ((s2,r2),None)::l;
                                          discard=g.discard}, won, false)
    else raise (Invalid_action "Can't deflect")
  end


(* returns a new gamestate in which all cards on the table have been added
 * to the hand of the active player
 * Precondition: the active player must be the defender *)
let take_all (g : state) : state =
  if g.active <> g.defender
    then raise (Invalid_action "You're not the defender!")
  else
    let table = tablepairs_to_list g.table in
    let p = {g.active with hand = table @ g.active.hand} in
    { g with active = p; defender = p; table = [] }


(* returns true iff the table is empty or at least one of the cards on the table
 * has the same rank as c *)
let valid_attack (g : state) ((s : suit) , (r : int)) : bool =
  g.table = [] || (List.exists (fun (_,x) -> x = r) (tablepairs_to_list g.table))


(* If the active player is an attacker holding card c, and card c is a valid
 * attack, remove that card from the active player's hand and add that card
 * to the table, then change the active player to either the next attacker
 * or to the defender, if the active player is the last attacker
 * and a bool telling whether the game has ended *)
let attack (g : state) (c : card) : state*bool*bool =
  if not (List.mem g.active g.attackers)
    then raise (Invalid_action "You are not an attacker!")
  else if not (valid_attack g c)
    then raise (Invalid_action "That is not a valid attack!")
  else if List.length g.table = 6
    then raise (Invalid_action "There are already 6 attacks on the table!")
  else let g' = game_play_card g c in
  let won = g'.active.hand = [] in
  let (g'',ended) = if won then do_win g' else (g',false) in
  if ended
    then ({g'' with table=[];
                    discard = (tablepairs_to_list g''.table) @ g''.discard},
                    won, ended)
  else
    let table' = (c,None)::g''.table in
      if won then ({g'' with table=table'; passed = []}, won, ended)
      else
        let active' =
          if g''.active.name = (List.hd g''.attackers).name then g''.defender
          else next_attacker g'' in
         ({ g'' with table = table'; active = active'; passed = []}, won, ended)


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


(* if c1 is unpaired on the table, and if c2 is a valid defense, return a new
 * gamestate with c2 paired to c1 on the table and with c2 removed from the
 * active player's hand.  Does not change the active player.
 * Raise Invalid_action otherwise. *)
let place_defense (g : state) (c1 : card) (c2 : card) : state =
  if not (List.mem (c1,None) g.table)
    then raise (Invalid_action ((string_of_card c1) ^ " is not on the table."))
  else if not (valid_defense c1 c2 g.trump)
    then let s1 = string_of_card c1 in
    let s2 = string_of_card c2 in
    raise (Invalid_action (s2 ^ " can't  defend against " ^ s1 ^ "! "))
  else let g' = game_play_card g c2 in
    let tabl = List.remove_assoc c1 g'.table in
    { g' with table = (c1,Some c2)::tabl }


(* Carries out "defend against c1 with c2" --
 * if the active player is the defender, c1 is an unanswered attack on the
 * table, c2 is a valid defense to c1, and c2 is in the active player's hand,
 * then [defend g c1 c2] removes card c2 from the active player's hand and
 * adds it to the table as a response to c1; otherwise Invalid_action is raised.
 * If all attacks on the table have been answered, the active player becomes
 * the attacker; otherwise the defender gets a chance to play again.
 * Makes defender a winner if c2 was her last card.
 * Also returns a bool telling whether or not g.active won and a bool telling
 * whether or not the game has ended *)
let defend (g : state) (c1 : card) (c2 : card) : state*bool*bool =
  if g.active.name <> g.defender.name
    then raise (Invalid_action "Only the defender can defend.") else
  if not (List.mem (c1,None) g.table)
    then raise (Invalid_action (string_of_card c1 ^ " is not on the table!")) else
  let g' = place_defense g c1 c2 in
  let won = (List.length g'.active.hand) = 0 in
  if won
    then let (g'',ended) = do_win g' in
    ({ g'' with table = []; passed = [] },won,ended) else
  ({g' with passed = []; active = last_attacker g'.attackers}, won, false)


(* returns true iff the two lists have exactly the same elements, though
 * they may be in different orders *)
let rec same_elements lst1 = function
  | [] -> lst1 = []
  | h::t ->
      if List.mem h lst1
      then let lst1' = List.filter (fun x -> x <> h) lst1 in
      same_elements lst1' t
      else false


(* [list_contains l1 l2] returns true iff all elements of l2 are members of l1*)
let rec list_contains l1 = function
  | [] -> true
  | h::t -> (List.mem h l1) && (list_contains l1 t)


(* makes the next player the active player. Raises Invalid_action if the
 * primary attacker tries to pass before any cards have been played. *)
let pass (g : state) : state =
  if g.table = [] && (g.active.name = (List.hd g.attackers).name) then
    raise (Invalid_action "You must attack. ") else
  let g' = {g with passed = (g.active::g.passed)} in
  if (list_contains g'.passed g'.attackers) && all_answered g'
    then new_turn {g' with active = g.defender} (last_attacker g'.attackers) else
  let active' =
    if g'.active.name = g'.defender.name
      then last_attacker g'.attackers
    else try next_attacker g' with Invalid_action _ -> g'.defender
  in {g' with active = active'}


(* returns the state that would result from applying c to g, as well as
 * a string describing what was done *)
let step (g:state) (c:command) : state*string*bool =
  try
    match c with
    | Attack c -> begin
        let (g',w,ended) = attack g c in
        let win_m = if w then g.active.name ^ " won! " else "" in
        let m=g.active.name^" attacked with "^string_of_card c^". " ^ win_m in
        (g',m,ended)
    end
    | Defend (c1,c2) -> begin
        let (g',w,ended) = defend g c1 c2 in
        let win_m = if w then g.active.name ^ " won! " else "" in
        let m=g.active.name^" defended with "^string_of_card c2^". " ^ win_m in
        (g',m,ended)
      end
    | Take -> begin
        let g' = take_all g in
        let next_defender = if List.length g'.attackers = 1 then g'.defender
                            else (penultimate g'.attackers) in
        let g'' = new_turn g' next_defender in
        let m = g.active.name ^ " chose to take." in
        (g'',m,false)
      end
    | Pass -> let m = g.active.name ^ " passed." in (pass g, m,false)
    | Deflect (c1,c2) -> begin
        let (g',w,e) = deflect g c1 c2 in
        let win_m = if w then g.active.name ^ " won! " else "" in
        let m=g.active.name^" deflected with "^string_of_card c2^". " ^ win_m in
        (g',m,e)
      end
  with
  | Invalid_action a -> begin
      match g.active.state with
      | Human -> (g, "\nThere was a problem: " ^ a, false)
      | CPU _ -> begin
          print_endline ("Invalid command: " ); print_command c;
          print_endline ("Problem: " ^ a);
          print_endline (g.active.name ^ " is a Durak! Surprise bonus win! ");
          exit 2;
        end
    end


(* ========================================================================== *)
(* ==============================TESTING===================================== *)
(* ========================================================================== *)

let field_compare g1 g2 =
  assert (g1.deck = g2.deck);
  assert (g1.trump = g2.trump);
  assert (g1.attackers = g2.attackers);
  assert (g1.defender = g2.defender);
  assert (g1.active = g2.active);
  assert (g1.table = g2.table);
  assert (g1.discard = g2.discard);
  assert (g1.winners = g2.winners);
  assert (g1.passed = g2.passed)

let sample_state () : state =
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
  let passed = [] in
  {deck=deck; trump=trump; attackers=attackers; defender=defender;
    table=table; active=active; discard=discard; winners=winners; passed=passed}

module Sample_state2 = struct
  let deck = [(Heart, 11); (Diamond, 6);  (Club, 10); (Club, 9)]
  let trump = Heart

  let hand1 = [(Heart, 7); (Heart, 6);  (Spade,13)]
  let player1 = {state = Human; hand = hand1; name = "Zapdoz"}

  let hand2 = [(Diamond, 6); (Club, 10); (Club, 12); (Spade,13); (Diamond, 14);
   (Club,11); (Spade,10); (Spade, 7); (Diamond, 8); (Heart, 10)]
  let player2 = {state= CPU(1); hand = hand2; name = "Rawr"}

  let hand3 = [(Diamond, 9)]
  let player3 = {state= CPU(1); hand = hand3; name = "Ayyy"}

  let hand4 = []
  let player4 = {state= CPU(1); hand = hand4; name = "Lmao"}

  let attackers = [player2; player3]
  let defender = player1
  let table = [((Club, 6), Some(Club,7));
               ((Diamond,6), Some(Diamond,10));
               ((Club,8), None);
               ((Heart,8), None);
               ((Spade, 8), Some(Spade,9))]

  let active = player2
  let discard = []
  let winners = [player4]
  let passed = []
  let game = {deck=deck; trump=trump; attackers=attackers; defender=defender;
    table=table; active=active; discard=discard; winners=winners; passed=passed}
end

let test_step () =
  let g1 = Sample_state2.game in
  let (g1',m,_) = step g1 (Attack (Spade, 7)) in
  let hand2' = [(Diamond, 6); (Club, 10); (Club, 12); (Spade,13); (Diamond, 14);
   (Club,11); (Spade,10); (Diamond, 8); (Heart, 10)] in
  let player2' = { Sample_state2.player2 with hand = hand2' } in
  let g1'' = { g1 with table = ((Spade,7),None)::g1.table;
                      active = Sample_state2.player1;
                   attackers = player2'::Sample_state2.player3::[];
                     discard = [] } in

  let (g2,s2,_) = step g1' (Defend ((Spade,7),(Spade,13))) in
  let defender' = {Sample_state2.player1 with hand = [(Heart, 7); (Heart, 6)]} in
  let g2' = { g1' with table = ((Spade,7),Some (Spade,13)) :: List.tl g1'.table;
                      active = Sample_state2.player3;
                    defender = defender'} in
  print_endline s2;
  field_compare g1' g1'';
  field_compare g2 g2'

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
  let passed = [] in
  let state1 = {deck=deck; trump=trump; attackers=attackers; defender=defender;
      table=table; active=active; discard=discard; winners=winners; passed=passed} in
  let dealt_state1 = deal state1 in
  let attacker' = List.nth dealt_state1.attackers 0 in

  assert (dealt_state1.deck          = [(Spade, 9)]);
  assert (dealt_state1.defender.hand = [(Club, 9); (Diamond, 9); (Heart, 7);
                                       (Diamond, 7); (Club,14); (Spade, 14)]);
  assert (attacker'.hand             = [(Heart, 9); (Diamond, 6); (Club, 10);
                                       (Club, 12); (Spade,13); (Diamond, 14)]);

  let state2 = sample_state () in
  let defender = state2.defender in
  let attacker1 = List.nth state2.attackers 0 in
  let attacker2 = List.nth state2.attackers 1 in
  let attacker3 = List.nth state2.attackers 2 in
  let dealt_state2 = deal state2 in
  let defender' = dealt_state2.defender in
  let attacker1' = List.nth dealt_state2.attackers 0 in
  let attacker2' = List.nth dealt_state2.attackers 1 in
  let attacker3' = List.nth dealt_state2.attackers 2 in

  assert (dealt_state2.deck = []);
  assert (defender'.hand = defender.hand);
  assert (attacker1'.hand = (Heart, 9)::attacker1.hand);
  assert (attacker2'.hand = (Spade, 9)::(Club, 9)::(Diamond, 9)::attacker2.hand);
  assert (attacker3'.hand = attacker3.hand)


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

let test_deflectable () =
  let table1 = [] in
  let table2 = [((Club, 13), None)] in
  let table3 = [((Heart, 12), None); ((Diamond, 12), None)] in
  let table4 = [((Heart, 12), None); ((Diamond, 12), Some(Heart, 8))] in
  let table5 = [((Club, 6), Some(Club,7));
               ((Diamond,6), Some(Diamond,10));
               ((Club,8), None);
               ((Heart,8), None);
               ((Spade, 8), Some(Spade,9))] in
  assert (not (deflectable 5 table1));
  assert (deflectable 13 table2);
  assert (not (deflectable 9 table2));
  assert (deflectable 12 table3);
  assert (not (deflectable 10 table3));
  assert (not (deflectable 12 table4));
  assert (not (deflectable 8 table5))

let test_change_active () =
  let state = sample_state () in
  let defender = state.defender in
  let attacker1 = List.nth state.attackers 0 in
  let attacker2 = List.nth state.attackers 1 in
  let attacker3 = List.nth state.attackers 2 in

  assert ((change_active state defender).active = defender);
  assert ((change_active state attacker1).active = attacker1);
  assert ((change_active state attacker2).active = attacker2);
  assert ((change_active state attacker3).active = attacker3)


let test_do_win () =
  let player3 = { Sample_state2.player3 with hand = [] } in
  let g0 = { Sample_state2.game with active = player3;
            attackers = [Sample_state2.player2; player3] } in
  let (g1,done1) = do_win g0 in
  let g1' = { g0 with attackers = [Sample_state2.player2];
            active = Sample_state2.player2;
            winners = [player3; Sample_state2.player4]} in
  let (g2,done2) = do_win g1 in
  let g2' = { g1 with attackers = []; active = Sample_state2.player1;
            winners = Sample_state2.player2::g1.winners } in
  assert (not done1);
  field_compare g1 g1' ;
  assert done2;
  field_compare g2 g2' ;
  ()

let test_take_all () =
  let g0 = { Sample_state2.game with active = Sample_state2.defender } in
  let g1 = take_all g0 in
  let hand1 = [(Club, 6); (Club,7); (Diamond,6); (Diamond,10); (Club,8);
      (Heart,8); (Spade, 8); (Spade,9); (Heart, 7); (Heart, 6);  (Spade,13)] in
  let p1 = { Sample_state2.player1 with hand = hand1 } in
  let g1' = { g0 with table = []; defender = p1; active = p1} in
  field_compare g1 g1' ;
  ()

let test_pass () =
  let g0 = Sample_state2.game in
  let g1 = pass g0 in
  let g1' = { g0 with active = Sample_state2.defender;
                      passed = [Sample_state2.active] } in
  field_compare g1 g1';
  let gs2 = {deck = [(Club, 8); (Spade, 13); (Spade, 12); (Spade, 10);
                      (Diamond, 6); (Club, 9); (Club, 6); (Spade, 11);
                      (Spade, 9); (Diamond, 12); (Club, 7)];
             trump = Club;
             attackers = [{state = Human;
                           hand = [(Heart, 13); (Spade, 6); (Spade, 8);
                                   (Diamond, 11); (Diamond, 9); (Heart, 14)];
                           name = "testplayer"};
                          {state = CPU 2;
                           hand = [(Club, 14); (Club, 13); (Club, 10);
                                   (Heart, 8); (Spade, 7); (Heart, 9)];
                           name = "Jose"};
                          {state = CPU 2;
                           hand = [(Club, 11); (Diamond, 13); (Club, 12);
                                   (Heart, 10); (Heart, 7); (Heart, 6)];
                           name = "Ivan"}];
             defender =   {state = CPU 2;
                           hand = [(Heart, 11); (Spade, 14); (Diamond, 10);
                                   (Diamond, 8); (Diamond, 7); (Heart, 12)];
                           name = "Mary"};
             table = [];
             active =     {state = CPU 2;
                           hand = [(Club, 11); (Diamond, 13); (Club, 12);
                                   (Heart, 10); (Heart, 7); (Heart, 6)];
                           name = "Ivan"};
             discard = [];
             winners = [];
             passed = [{state = Human;
                           hand = [(Heart, 13); (Spade, 6); (Spade, 8);
                                   (Diamond, 11); (Diamond, 9); (Heart, 14)];
                           name = "testplayer"};
                          {state = CPU 2;
                           hand = [(Club, 14); (Club, 13); (Club, 10);
                                   (Heart, 8); (Spade, 7); (Heart, 9)];
                           name = "Jose"};];} in
  let gs3 = {gs2 with  passed = [];
                    attackers = [{state = CPU 2;
                                  hand = [(Heart, 11); (Spade, 14); (Diamond, 10);
                                   (Diamond, 8); (Diamond, 7); (Heart, 12)];
                                  name = "Mary"};
                                  {state = Human;
                                  hand = [(Heart, 13); (Spade, 6); (Spade, 8);
                                   (Diamond, 11); (Diamond, 9); (Heart, 14)];
                                  name = "testplayer"};
                                  {state = CPU 2;
                                   hand = [(Club, 14); (Club, 13); (Club, 10);
                                   (Heart, 8); (Spade, 7); (Heart, 9)];
                                   name = "Jose"};];
                    defender =   {state = CPU 2;
                                  hand = [(Club, 11); (Diamond, 13); (Club, 12);
                                   (Heart, 10); (Heart, 7); (Heart, 6)];
                                  name = "Ivan"};
                    active =     {state = CPU 2;
                                  hand = [(Heart, 11); (Spade, 14); (Diamond, 10);
                                   (Diamond, 8); (Diamond, 7); (Heart, 12)];
                                  name = "Mary"};} in
  field_compare (pass gs2) gs3;
  ()

let run_tests () =
  test_do_win ();
  test_string_of_card ();
  test_init_deck ();
  test_init_game_state ();
  test_play_card ();
  test_valid_defense ();
  test_deal ();
  test_last_attacker ();
  test_remove_last ();
  test_pass ();
  test_deflectable ();
  test_change_active ();
  test_step ();
  test_take_all ();
  test_pass ();
  print_endline "all tests pass";
  ()

(*let _ = run_tests ()*)
