open Cards
open Command

(*Algorithms for the AI opponents in Durak. Stubs and guidance for
 *the Hard AI's decision algorithm sourced from:
 *
 *http://www.aifactory.co.uk/newsletter/ISMCTS.txt
 *Written by Peter Cowling, Edward Powley, Daniel Whitehouse
 *(University of York, UK) September 2012 - August 2013.*)

(*****************************************************************************)
(***********************************HELPERS***********************************)
(*****************************************************************************)

let suit_to_string (suit: suit) : string =
  match suit with
  | Heart   -> "H"
  | Club    -> "C"
  | Diamond -> "D"
  | Spade   -> "S"

let rank_to_string (rank: int) : string =
  match rank with
  | 14 -> "A"
  | 13 -> "K"
  | 12 -> "Q"
  | 11 -> "J"
  | n  -> string_of_int n

let card_to_string c : string =
  match c with
  | (a,b) -> " |"^(suit_to_string a)^(rank_to_string b)^"| "

let command_to_string c : string =
  match c with
  | Pass -> "Pass\n"
  | Take -> "Take\n"
  | Deflect (a,b) ->
     "Deflect "^(card_to_string a)^"with"^(card_to_string b)^"\n"
  | Attack a -> "Attack with"^(card_to_string a)^"\n"
  | Defend (a,b) -> "Defend "^(card_to_string a)^"with"^(card_to_string b)^"\n"

let rec printCardList d : string =
  match d with
  | [] -> ""
  | (a,b)::tl -> (suit_to_string a)^":"^
                 (rank_to_string b)^" | "^(printCardList tl)

let rec printPlayerHands lst : string =
  match lst with
  | [] -> ""
  | hd::tl -> "ATTACKER: "^(printCardList hd.hand)^"\n"^(printPlayerHands tl)

let rec printPlayers lst : string =
  match lst with
  | [] -> ""
  | hd::tl -> hd.name^", "^(printPlayers tl)

let rec printCommList lst : string =
  match lst with
  | [] -> ""
  | hd::tl -> (command_to_string hd)^(printCommList tl)

(*unwrap option*)
let unwrap o =
  match o with
  | None -> failwith "Something unwrapped poorly"
  | Some a -> a

(*version of Pervasives.compare which ignores card suits*)
let cardCompare ((a,b):card) ((c,d):card) = compare b d


(* [sortHand a h t] outputs [h] sorted in increasing order, filtered by suit
* of [a] and [t], and by rank of [a] *)
let sortHand (hand:deck) (attack:card) (trump:suit) : deck =
  List.sort cardCompare
    (List.filter
      (fun (a,b) ->
        if a = (fst attack) || b = (snd attack) || a = trump
        then true
        else false)
      hand)


(* [lowestValidDefOf h a t] outputs the card in [h] of rank >= rank of [a] that
* matches either suit of [a] or suit of [t] *)
let lowestValidDefOf (hand:deck) (attack:card) (trump:suit) : card option =
  let result = sortHand hand attack trump in
  let rec matchResult lst =
    match lst with
    | [] -> None
    | (a,b)::tl -> if b >= (snd attack)
                then Some (a,b)
                else
                  (if a = trump
                   then Some (a,b)
                   else matchResult tl) in
  matchResult result

(*returns a list of the undefended card pairs*)
let rec getUndefended (t:(card*card option) list) : card list =
  match t with
  | [] -> []
  | (a,b)::tl -> if b = None
                 then a::(getUndefended tl)
                 else getUndefended tl

(*returns true if att is a valid attack, given table inPlay*)
let isValidAtt (inPlay:(card*card option) list) (att:card) : bool =
  if List.length inPlay = 0
    then true
  else
    begin
      let rec compRanks pairlst =
        match pairlst with
        | [] -> []
        | (a,None)::tl -> (snd a)::(compRanks tl)
        | (a, Some b)::tl -> (snd a)::(snd b)::(compRanks tl) in
      List.mem (snd att) (compRanks inPlay)
    end

(*[isValidDef a d t] returns true if d is a valid defense against a. Does not
 *include deflections *)
let isValidDef (attack:card) (defend:card) (trump:suit) : bool =
  if (fst attack) = (fst defend) then (snd attack) < (snd defend)
  else if (fst defend) = trump then true
  else false

(*returns true if def is a valid deflection, given state g*)
let isValidDeflect (def:card) (g:state) =
  if g.active <> g.defender then false
  else if List.length g.table <> 1 then false
  else if (snd def) <> (snd (fst (List.hd g.table))) then false
  else true

(*returns true if Pass is a valid command given state g*)
let isValidPass (g:state) =
  (List.mem g.active g.attackers)

(*returns true if Take is a valid command, given state g*)
let isValidTake (g:state) =
  g.active = g.defender

(*removes duplicate elements from a list*)
let rec rem_dups lst =
  match lst with
  | [] -> []
  | hd::tl -> hd::(rem_dups (List.filter (fun a -> a<>hd) tl))

(*returns a list of all valid Defend commands given state g*)
let rec getValidDefenses (g:state) : command list =
  if List.length g.attackers = 0
    then []
  else if List.mem g.active g.attackers
    then []
  else
    begin
      let rec itrHand atk lst1 =
        match lst1 with
        | [] -> []
        | hd::tl -> if isValidDef atk hd g.trump
                    then (Defend (atk,hd))::(itrHand atk tl)
                    else itrHand atk tl in
      let rec itrAtks lst2 =
        match lst2 with
        | [] -> []
        | hd::tl -> (itrHand hd g.active.hand) @ (itrAtks tl) in
      itrAtks (getUndefended g.table)
    end

(*returns a list of all valid Attack commands, given state g*)
let rec getValidAttacks (g:state) : command list =
  if List.length g.attackers = 0
    then []
  else if g.active = g.defender
    then []
  else
    begin
      let rec itrHand lst1 =
        match lst1 with
        | [] -> []
        | hd::tl -> if isValidAtt g.table hd
                      then (Attack hd)::(itrHand tl)
                    else itrHand tl in
      itrHand g.active.hand
    end

(*returns a list of valid Deflect commands, given state g*)
let rec getValidDeflections (g:state) : command list =
  if List.length g.attackers = 0
    then []
  else if List.length g.table = 0
    then []
  else
    begin
      let rec itrHand atk lst1 =
        match lst1 with
        | [] -> []
        | hd::tl -> if isValidDeflect hd g
                    then (Deflect (atk,hd))::(itrHand atk tl)
                    else itrHand atk tl in
      itrHand (fst (List.hd g.table)) g.active.hand
    end

(*[firstUndefended t] outputs the first undefended attacking card in [t]*)
let rec firstUndefended (table:(card * card option) list) =
  match table with
  | [] -> failwith "defend called on empty table or fully defended table"
  | (a,b)::tl -> if b = None
                 then a
                 else firstUndefended tl


(*returns a list of the undefended card pairs*)
let rec getUndefended (table:(card * card option) list) : card list =
  match table with
  | [] -> []
  | (a,b)::tl -> if b = None
                 then a::(getUndefended tl)
                 else getUndefended tl


(*Provides functions for manipulating game states.
 * Stubs sourced from: http://www.aifactory.co.uk/newsletter/ISMCTS.txt*)
 module GameState = struct
  (*[GetNextPlayer g] returns the next player to move in gameState [g]*)
  let getNextPlayer (g:state) =
    failwith "TODO"

  (*returns a new deck of cards
   *Source: http://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml*)
  let rec getCardDeck d n =
    if n < 15
    then let new_d = List.append d [(Heart, n); (Diamond, n);
                                 (Spade, n); (Club, n)] in
        getCardDeck new_d (n+1)
    else d

  (*shuffle card deck d*)
  let shuffle d =
    let nd = List.map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort compare nd in
    List.map snd sond


  (*Reset the game state for the beginning of a new round, and deal the cards.*)
  let deal g =
    let deck = shuffle (getCardDeck [] 6) in
    let players = g.defender::g.attackers in

    (*make sure player hands are emptied prior to calling*)
    let rec dealOnce deck player iter =
      match deck with
      | [] -> failwith "delt with too few cards"
      | hd::tl -> if iter = 0 then (deck,player)
                  else let newHand = hd::player.hand in
                       dealOnce tl {player with hand = newHand} (iter-1) in
    let rec dealAll deck playerList newlst =
      if playerList = [] then (deck,newlst) else
      match dealOnce deck (List.hd playerList) 6 with
      | (a,b) -> dealAll a (List.tl playerList) (b::newlst) in
    let rec prepPlayers (lst:player list) =
      match lst with
      | [] -> []
      | hd::tl -> {hd with hand = []}::(prepPlayers tl) in

    let (newDeck,newPlayers) = dealAll deck (prepPlayers players) [] in
    let newPlayers = List.rev newPlayers in
    let newDiscards = [] in
    let newTable = [] in
    let newTrump = List.hd (shuffle [Heart; Club; Spade; Diamond]) in
    {g with discard = newDiscards;
            table = newTable;
            trump = newTrump;
            deck = newDeck;
            defender = List.hd newPlayers;
            attackers = List.tl newPlayers;
            active = List.hd (List.tl newPlayers)
            }

  (*[Clone g] returns a clone of gameState [g]*)
  let clone g =
    failwith "TODO"

  let rec unwrapTable (lst:(card * card option) list) =
    match lst with
    | [] -> []
    | (a,None)::tl -> a::(unwrapTable tl)
    | (a, Some b)::tl -> a::b::(unwrapTable tl)

  (*[CloneAndRandomize g p] returns a clone of the given gameState [g], after
   *randomizing the elements that are invisible to the given player [p]*)
  let cloneAndRandomize (g:state) =
    let p = g.active in
    let () = print_endline ("PLAYERHAND: "^(string_of_int (List.length p.hand))); in
    let players = g.defender::g.attackers in
    (*let nonActive = List.filter (fun a -> a <> p) players in*)
    let seenCards = p.hand @ g.discard @ (unwrapTable g.table) in
    let unseenCards = List.filter (fun a -> not (List.mem a seenCards))
                                  (shuffle (getCardDeck [] 6)) in
    let rec dealO deck player iter =
      match deck with
      | [] -> (deck,player)
      | hd::tl -> if iter = 0 then (deck,player)
                  else let newHand = hd::player.hand in
                       dealO tl {player with hand = newHand} (iter-1) in

    let rec dealA deck playerList newlst =
      (if playerList = [] then (deck,(List.rev newlst))
      else let hd = List.hd playerList in
        if hd = p
          then dealA deck (List.tl playerList) (hd::newlst)
        else
          match
            dealO deck hd (6-(List.length hd.hand))
          with
          | (a,b) -> dealA a (List.tl playerList) (b::newlst))
    in
    let rec prep (lst:player list) =
      match lst with
      | [] -> []
      | hd::tl -> if hd = p
                    then ({hd with hand = p.hand})::(prep tl)
                  else ({hd with hand = []})::(prep tl) in

    let (newDeck, newPlayers) = dealA unseenCards (prep players) [] in
    {g with deck= newDeck;
            defender= (List.hd newPlayers);
            attackers= (List.tl newPlayers)}


  (*DoMove c g] updates gameState [g] by executing command [c] *)
  let doMove command gameState =
    let (m,_) = step gameState command in m

  (*[GetMoves g] returns all possible moves given gameState g*)
  let getMoves (g:state) : command list =
    (getValidAttacks g) @ (getValidDefenses g) @ (getValidDeflections g) @
      (if g.active <> g.defender || List.length g.attackers = 0
        then [] else [Take]) @
      (if not (List.mem g.active g.attackers) || List.length g.attackers = 0
        then [] else [Pass])

  (*[GetResult g p] returns the result of gameState [g] from point of view of
   *player [p]*)
  let getResult g p =
    if (List.mem p g.winners) then 1 else 0
end

type node = {
               thisMove: command option;
               parent: node option;
               children: node list;
               wins: int ref;
               visits: int ref;
               avails: int ref;
               playerJustMoved: player option;
              }

(*Provides functions for creating and manipulating nodes in the ISMCTS
 *  algorithm tree.
 *Stubs sourced from: http://www.aifactory.co.uk/newsletter/ISMCTS.txt*)
module Node = struct

  (*[GetUntriedMoves lms] returns the elements of lms for which this node does
   *not have children**)
  let getUntriedMoves (legalMoves:command list) (n:node) : command list =
    let rec triedMoves = function
      | [] -> []
      | hd::tl -> (unwrap hd.thisMove)::(triedMoves tl) in
    List.filter (fun a -> not (List.mem a (triedMoves n.children))) legalMoves

  let uCBFunction (n:node) =
     (float_of_int !(n.wins)) /. (float_of_int !(n.visits)) +.
      0.7 *.
      sqrt(log((float_of_int !(n.avails))) /.
      (float_of_int !(n.visits)))

  let rec incrAvails n =
    let rec helper lst =
      match lst with
      | [] -> ()
      | hd::tl -> incr hd.avails; helper tl in
    helper n.children

  (*[UCBSelectChild lms d] uses the UC1 formula to select a child node, filtered
   *by the given list of legal moves [lms] and exploration coefficient [d]*)
  let uCBSelectChild (legalMoves:command list) n =
    let rec legalChildren lst =
      match lst with
      | [] -> []
      | hd::tl -> if List.mem (unwrap hd.thisMove) legalMoves
                  then hd::(legalChildren tl)
                 else legalChildren tl in
    let rec choice maxSoFar lst =
      match lst with
      | [] -> maxSoFar
      | hd::tl -> if (uCBFunction hd > uCBFunction maxSoFar)
                    then choice hd tl
                  else choice maxSoFar tl in
    incrAvails n; choice (List.hd (legalChildren n.children))
      (legalChildren n.children)

  (*[AddChild m n p] adds a new child node to Node [n] for the move [m] with
   *playerJustMoved set to [p]*)
  let addChild (m:command) n p : node =
    let newChild = {thisMove = Some m;
                    parent = Some n;
                    children = [];
                    wins = ref 0;
                    visits = ref 0;
                    avails = ref 0;
                    playerJustMoved = Some p;
                    } in
    let newChildren = (newChild::n.children) in
    {n with children=newChildren}


  (*[Update n s] increment the visit count of node [n], increase win count of
   *[n] by the result of [GetResult p] for active player [p] of [g]*)
  let update n g : unit =
    incr n.visits;
    match n.playerJustMoved with
    | None -> ()
    | Some a -> n.wins := (!(n.wins) + (GameState.getResult g a)); ()

  (*string representation of tree for debugging purposes*)
  let treeToSTring t =
    failwith "TODO"

end


(******************************************************************************)
(********************************ISMCTS Helpers********************************)
(******************************************************************************)

(*return a randomly selected command from [lst]*)
let randomMove lst =
  let nd = List.map (fun c -> (Random.bits (), c)) lst in
    let sond = List.sort compare nd in
    List.hd (List.map snd sond)

(*if [g1] is non-terminal, and [n] has no untried moves, select a child and
 *descend the tree*)
let rec select g1 n =
      let moves = (GameState.getMoves g1) in
      if moves <> [] && (Node.getUntriedMoves moves n) = []
        then begin
          let newNode = (Node.uCBSelectChild moves n) in
          let newState = (GameState.doMove (unwrap newNode.thisMove) g1) in
          select newState newNode
        end
      else (g1,n)

(*[g1_1 n untried] If [untried] is non_empty, select a random element, step [g1_1], create a new
 *node and return it with associated state*)
let expand g1_1 n untried =
  if untried <> []
    then begin
      let m = randomMove untried in
      let plyr = g1_1.active in
      let newNode =
        List.hd (Node.addChild m n plyr).children in
        let new_state = GameState.doMove m g1_1 in
      (new_state,newNode)
    end
  else (g1_1,n)

(*simulate a game, through to completion, making random choices, return *)
let rec simulate g2 =
      let moves = GameState.getMoves g2 in
      match moves with
      | [] -> g2
      | _ -> simulate (GameState.doMove (randomMove moves) g2)

(*[g2_1 n1] Work back up the tree, updating each node to reflect outcomes*)
let rec backPropogate g2_1 n1 =
      match n1.parent with
      | None -> n1
      | _ -> Node.update n1 g2_1; backPropogate g2_1 (unwrap n1.parent)

(*return the child with the most visits*)
let rec maxChild max lst =
        match lst with
        | [] -> max
        | hd::tl -> if !(hd.visits) > !(max.visits)
                      then maxChild hd tl
                    else maxChild max tl

(*Information Set Monte Carlo Tree Search algorithm*)
let iSMCTS g itermax =
  let rootnode = {
                  thisMove = None;
                  parent = None;
                  children = [];
                  wins = ref 0;
                  visits = ref 0;
                  avails = ref 0;
                  playerJustMoved = None;
                 }

  in

  let rec forLoop root itermax1 =
    if itermax <> 0 then
      begin
        let node = root in
        let state = (GameState.cloneAndRandomize g) in

        (*SELECT*)
        let (state,node)= select state node in
        let untriedMoves = Node.getUntriedMoves (GameState.getMoves state)  node in
        (*EXPAND*)
        let (state,node) = expand state node untriedMoves in
        (*SIMULATE*)
        let state = (simulate state) in
        (*BACK PROPOGATE*)
        let node = backPropogate state node in
        forLoop node (itermax1 - 1)
      end
    else
      unwrap ((maxChild (List.hd root.children) root.children).thisMove)
  in

  forLoop rootnode itermax


module Easy = struct
  (*The easy AI's defending function*)
  let easyDefend (gameState:state) : command =
    let hand = gameState.active.hand in
    let table = gameState.table in
    let trump = gameState.trump in
    let attack = firstUndefended table in
    let result = lowestValidDefOf (hand) (attack) (trump) in
    match result with
    | None -> Take
    | Some (a,b) -> if List.length table = 1 &&
                       b = (snd attack)
                      then Deflect (attack, (a,b))
                    else Defend (attack, (a,b))


  (*The easy AI's attacking function*)
  let easyAttack (gameState:state) : command =
    let sortedHand = List.sort cardCompare gameState.active.hand in
    let table = gameState.table in
    let len = List.length table in
    let inPlay = List.map (fun ((a,b),c) -> b) (table) in
    let rec validAttack lst =
      match lst with
      | [] -> Pass
      | hd::tl -> if List.mem (snd hd) inPlay then Attack hd else validAttack tl in
    if len = 6 then Pass
    else if len = 0 then Attack (List.hd sortedHand)
    else validAttack sortedHand


  (*Easy AI branch*)
  let easy (gameState:state) : command =
    if gameState.active = gameState.defender
      then easyDefend gameState
    else if List.mem gameState.active gameState.attackers
      then easyAttack gameState
    else failwith "error: AI player neither a defender nor an attacker"
end


module Medium = struct

  type game_stage = | Early | Late

  let get_game_stage (gs:state) : game_stage =
    let deck_len = List.length (gs.deck) in
    if deck_len > 0
      then Early
    else Late

  (* Helper for calculating defense. Will output "lowest" valid defense,
   * giving preference to low rank and to the same suit as the attack
   * over the trump suit, if different. best_def h a t outputs the best
   * defense of attack a in hand h where the trump suit is t. *)
  let best_def (hand:deck) (att:card) (trump:suit) : card option =
    let valid_defs = fun (s, v) ->
                       if (fst att) = trump
                         then (s = trump) && (v > (snd att))
                       else (s = trump) || (s = fst att && v > snd att) in
    let filtered_hand = List.filter valid_defs hand in
    let rec get_best_def filt_hand current_def = (
      match filt_hand with
      | [] -> current_def
      | (s,v)::tl -> (match current_def with
                     | None -> get_best_def tl (Some (s,v))
                     | Some (s', v') -> if s' = trump
                                          then if (s <> trump) || (v < v')
                                                 then get_best_def tl (Some (s,v))
                                               else get_best_def tl current_def
                                        else if (v < v') && (s <> trump)
                                               then get_best_def tl (Some (s,v))
                                             else get_best_def tl current_def)
    ) in get_best_def filtered_hand None

(*   let can_defl (hand:deck) (table:(card*card option) list) : bool =
    if List.length table = 0
      then false
    else let val = fst (List.hd table) in *)


  let best_deflection (hand:deck) (att:card) (gs:state): card option =
    let possible_defls = List.filter (fun (a,b) -> (snd att) = b) hand in
    let rec get_best_defl filt_hand current_defl =
      match filt_hand with
      | [] -> current_defl
      | (s,v)::tl -> (match current_defl with
                      | None -> get_best_defl tl (Some (s,v))
                      | Some (s',v') -> if s' = gs.trump
                                          then get_best_defl tl (Some (s,v))
                                        else get_best_defl tl current_defl) in
    if List.length gs.table = 1
      then get_best_defl possible_defls None
    else None

  (* [def_all_attacks h c g l] returns list of defenses in response to cards in
  * [c] from [h]. Not necessarily in the correct order. If a card cannot be
  * defended, no value will be added to the list. *)
  let rec def_all_attacks (hand:deck) (cs:card list) (gs:state) (lst:card list)
                        : card list =
    match cs with
    | [] -> lst
    | hd::tl -> (
      match best_def hand hd (gs.trump) with
      | None -> lst
      | Some c -> def_all_attacks (List.filter (fun x -> x <> c) hand) tl
                                  gs (List.append lst [c]) )

  (* Medium AI defense calculator for early game*)
  let early_def (cs:card list) (gs:state) : command =
    (*One card to defend - deflect or defend if low, take if high*)
    if (List.length cs = 1)
      then if (snd (List.hd cs) >= 11 || fst (List.hd cs) = gs.trump)
             then Take
           else
             let at = List.hd cs in
             match best_deflection (gs.active).hand at gs with
             | None -> (match best_def (gs.active).hand at (gs.trump) with
                        | None -> Take
                        | Some (a,b) -> Defend (at, (a,b)))
             | Some c -> Deflect (at, c)
    (*Multiple cards on the table*)
    else let def_list = def_all_attacks ((gs.active).hand) cs gs [] in
         if List.length def_list = List.length cs
           then let is_high = fun x -> (fst x) = gs.trump || (snd x) > 10 in
             if (float_of_int (List.length (List.filter is_high def_list)) /.
                float_of_int (List.length def_list)) >= 0.5
               then Take (*Take if more than >= half are high or trump*)
             else let first = List.hd cs in
                  (match best_def (gs.active).hand first (gs.trump) with
                   | None -> Take
                   | Some c -> Defend (first, c)) (*Defend low cards if can*)
         else Take (*Take if not valid moves for all cards on table*)

  (* Medium AI defense calculator for late game*)
  let late_def (cs:card list) (gs:state) : command =
    if List.length cs = 1
      then let at = List.hd cs in
        match best_deflection (gs.active).hand at gs with
             | None -> (match best_def (gs.active).hand at (gs.trump) with
                        | None -> Take
                        | Some (a,b) -> Defend (at, (a,b)))
             | Some c -> Deflect (at, c)
    else let first = List.hd cs in
         (match best_def (gs.active).hand first (gs.trump) with
          | None -> Take
          | Some c -> Defend (first, c))


  let med_defend (gs:state) : command =
    let attacks = getUndefended (gs.table) in
    match (get_game_stage gs) with
    | Early -> early_def attacks gs
    | Late -> late_def attacks gs

  (* Sorts hand by increasing numerical value, with trump cards all at the end*)
  let sort_hand (h:deck) (tr:suit) =
    let num_sorted = List.sort cardCompare h in
    let rec trump_sort non_trumps trumps old_hand = (
      match old_hand with
      | [] -> non_trumps @ trumps
      | (s,v)::tl -> if s = tr
                       then trump_sort non_trumps (trumps @ [(s,v)]) tl
                     else trump_sort (non_trumps @ [(s,v)]) trumps tl ) in
    trump_sort [] [] num_sorted

  let early_att (gs:state) : command =
    let sorted_hand = sort_hand (gs.active).hand gs.trump in
    if List.length (gs.table) = 0
      then Attack (List.hd sorted_hand)
    else if List.length (gs.table) < 6
      then let valid_attacks = List.filter (isValidAtt gs.table) sorted_hand in
           match valid_attacks with
           | [] -> Pass
           | (s,v)::tl -> if (s = gs.trump) || (v >= 12)
                            then Pass
                          else Attack (s,v)
    else Pass

  let late_att (gs:state) : command =
    let sorted_hand = sort_hand (gs.active).hand gs.trump in
    if List.length (gs.table) = 0
      then Attack (List.hd sorted_hand)
    else if List.length (gs.table) < 6
      then let valid_attacks = List.filter (isValidAtt gs.table) sorted_hand in
           match valid_attacks with
           | [] -> Pass
           | hd::tl -> Attack hd
    else Pass

  let med_attack (gs:state) : command =
    match (get_game_stage gs) with
    | Early -> early_att gs
    | Late -> late_att gs

  let medium (gs:state) : command =
    if gs.active = gs.defender
      then med_defend gs
    else if List.mem gs.active gs.attackers
      then med_attack gs
    else failwith "error: AI player neither a defender nor an attacker"

end


module Hard = struct
  let hard (gameState:state) : command =
    iSMCTS gameState 100
end


let response (gameState:state) : command =
  match gameState.active.state with
  | CPU 1 -> Easy.easy gameState
  | CPU 2 -> Medium.medium gameState
  | CPU 3 -> Hard.hard gameState
  | _ -> failwith "[response] error. Invalid active player_state"

let testStateInit () =
  let deck = GameState.shuffle (GameState.getCardDeck [] 6) in
  let trump = Heart in
  let defender = {state= CPU 3; hand= []; name= "Foo"} in
  let attackers = [
    {state= CPU 3; hand= []; name= "Bar"};
    {state= CPU 3; hand= []; name= "Bob"};
    {state= CPU 3; hand= []; name= "Blarg"};
  ] in
  let table = [] in
  let active = List.hd attackers in
  let discard = [] in
  let winners = [] in
  let g = {
    deck; trump; defender; attackers; table; active; discard; winners
  } in
  let g1 = GameState.deal g in
  g1
(*****************************************************************************)
(************************************TESTS************************************)
(*****************************************************************************)

let test_lowestValidDefOf () =
  let testTrump = Diamond in
  let testAttack = (Heart, 9) in
  let testHand1 = [(Heart, 7);(Heart, 8);(Spade, 7);(Club, 8)] in
  let testHand2 = [(Heart, 7);(Heart, 8);(Diamond, 6);(Club, 8)] in
  let testHand3 = [(Club, 7);(Club, 8);(Spade, 7);(Club, 11)] in
  let testHand4 = [(Club, 7);(Club, 8);(Heart, 10);(Diamond, 11)] in
  let testHand5 = [] in
  let testHand6 = [(Club, 7);(Club, 9);(Heart, 10);(Diamond, 11)] in
  assert (lowestValidDefOf testHand1 testAttack testTrump = None);
  assert (lowestValidDefOf testHand2 testAttack testTrump = Some (Diamond, 6));
  assert (lowestValidDefOf testHand3 testAttack testTrump = None);
  assert (lowestValidDefOf testHand4 testAttack testTrump = Some (Heart, 10));
  assert (lowestValidDefOf testHand5 testAttack testTrump = None);
  assert (lowestValidDefOf testHand6 testAttack testTrump = Some (Club, 9));
  ()

let test_easy_defend () =
  let deck = [] in
  let trump = Diamond in

  let testHand1 = [(Heart, 7);(Heart, 8);(Spade, 7);(Club, 8)] in
  let testHand2 = [(Heart, 7);(Heart, 8);(Diamond, 6);(Club, 8)] in
  let testHand3 = [(Club, 7);(Club, 8);(Spade, 7);(Club, 11)] in
  let testHand4 = [(Club, 7);(Club, 8);(Heart, 10);(Diamond, 11)] in
  let testHand5 = [] in
  let testHand6 = [(Club, 7);(Club, 9);(Heart, 10);(Diamond, 11)] in
  let player1 = {state = CPU 1; hand = testHand1; name = "happy1"} in
  let player2 = {state = CPU 1; hand = testHand2; name = "happy2"} in
  let player3 = {state = CPU 1; hand = testHand3; name = "happy3"} in
  let player4 = {state = CPU 1; hand = testHand4; name = "happy4"} in
  let player5 = {state = CPU 1; hand = testHand5; name = "happy5"} in
  let player6 = {state = CPU 1; hand = testHand6; name = "happy6"} in

  let hand2 = [(Diamond, 6); (Club, 10); (Club, 12); (Spade,13); (Diamond, 14)] in
  let attacker = {state= CPU(1); hand = hand2; name = "Dora"} in

  let attackers = [attacker] in
  let defender = player1 in
  let table = [((Club, 6), Some(Club,7));
               ((Diamond,6), Some(Diamond,8));
               ((Heart,9), None);
               ((Heart,8), None);
               ((Spade, 8), Some(Spade,9))] in

  let active = player1 in
  let discard = [] in
  let winners = [] in
  let state1 = {deck=deck; trump=trump; attackers=attackers; defender=defender;
               table=table; active=active; discard=discard; winners=winners} in
  let state2 = {state1 with defender=player2; active=player2} in
  let state3 = {state1 with defender=player3; active=player3} in
  let state4 = {state1 with defender=player4; active=player4} in
  let state5 = {state1 with defender=player5; active=player5} in
  let state6 = {state1 with defender=player6; table=[((Heart,9), None)];
                            active=player6} in

  assert (Easy.easy state1 = Take);
  assert (Easy.easy state2 = Defend ((Heart,9),(Diamond, 6)));
  assert (Easy.easy state3 = Take);
  assert (Easy.easy state4 = Defend ((Heart,9),(Heart, 10)));
  assert (Easy.easy state5 = Take);
  assert (Easy.easy state6 = Deflect ((Heart,9),(Club, 9)));
  ()

let test_easy_attack () =
  (*let deck = [] in
  let trump = Diamond in

  let attackers =
  let defender = {state=CPU 1; hand=[]}
  let table = [((Heart, 7), None);((Club, 9), None);((Diamond, 6), None)]
  let active =
  let discard =
  let winners =*)
  failwith "TODO"

let test_gameState_shuffle () =
  let d = GameState.getCardDeck [] 10 in
  let d1 = GameState.shuffle d in
  print_endline (printCardList d);
  print_endline (printCardList d1);
  ()

let test_gameState_cloneAndRandomize () =
  let () = print_endline "testing cloneAndRandomize: "; in
  let g1 = testStateInit () in
  print_endline ("DECK: "^(printCardList g1.deck));
  print_endline ("DEFENDER: "^(printCardList g1.defender.hand));
  print_endline (printPlayerHands g1.attackers);
  let g2 = GameState.cloneAndRandomize g1 in
  print_endline ("DECK: "^(printCardList g2.deck));
  print_endline ("DEFENDER: "^(printCardList g2.defender.hand));
  print_endline (printPlayerHands g2.attackers);
  print_endline "";
  ()

let test_gameState_doMove () =
  let () = print_endline "testing doMove: " in
  let g1 = testStateInit () in
  let moveList = GameState.getMoves g1 in
  print_endline ("DECK: "^(printCardList g1.deck));
  print_endline ("DEFENDER: "^(printCardList g1.defender.hand));
  print_endline (printPlayerHands g1.attackers);
  print_endline (printCommList moveList);
  print_endline ("ACTIVE: "^g1.active.name);
  let g2 = GameState.doMove (Attack (Spade,7)) g1 in
  print_endline ("DECK: "^(printCardList g2.deck));
  print_endline ("DEFENDER: "^(printCardList g2.defender.hand));
  print_endline (printPlayerHands g2.attackers);
  print_endline ("ACTIVE: "^g2.active.name);
  ()

let test_gameState_getMoves () =
  let () = print_endline "testing getMoves: " in
  let g1 = testStateInit () in
  let moveList = GameState.getMoves g1 in
  print_endline ("DECK: "^(printCardList g1.deck));
  print_endline ("DEFENDER: "^(printCardList g1.defender.hand));
  print_endline (printPlayerHands g1.attackers);
  print_endline (printCommList moveList);
  let g2 = GameState.cloneAndRandomize g1 in
  let g3 = {g2 with active = g2.defender;
                    table = [((Diamond,10),None)]} in
  let moveList1 = GameState.getMoves g3 in
  print_endline ("DECK: "^(printCardList g3.deck));
  print_endline ("DEFENDER: "^(printCardList g3.defender.hand));
  print_endline (printPlayerHands g3.attackers);
  print_endline (printCommList moveList1);
  ()

let test_gameState_getResult () =
  failwith "TODO"

let test_Node_getUntriedMoves () =
  failwith "TODO"

let test_Node_uCBSelectChild () =
  failwith "TODO"

let test_Node_addChild () =
  failwith "TODO"

let test_Node_update () =
  failwith "TODO"

(*if [g1] is non-terminal, and [n] has no untried moves, select a child and
 *descend the tree*)
let test_select () =
  let () = print_endline "testing select: " in
  let g1 = testStateInit () in
  print_endline ("DECK: "^(printCardList g1.deck));
  print_endline ("DEFENDER: "^(printCardList g1.defender.hand));
  print_endline (printPlayerHands g1.attackers);
  let n = {
            thisMove = None;
            parent = None;
            children = [];
            wins = ref 0;
            visits = ref 0;
            avails = ref 0;
            playerJustMoved = None;
          } in
  let _ = (select g1 n); in
  ()

(*[g1_1 n untried] If [untried] is non_empty, select a random element, step [g1_1], create a new
 *node and return it with associated state*)
let test_expand () =
  let () = print_endline "testing expand: " in
  let g1 = testStateInit () in
  let n = {
            thisMove = None;
            parent = None;
            children = [];
            wins = ref 0;
            visits = ref 0;
            avails = ref 0;
            playerJustMoved = None;
          } in
  let untried1 = Node.getUntriedMoves (GameState.getMoves g1) n in
  let _ = (expand g1 n untried1); in
  ()

(*simulate a game, through to completion, making random choices, return *)
let rec test_simulate () =
  let () = print_endline "testing simulate: " in
  let g1 = testStateInit () in
  let _ = (simulate g1); in
  ()

(*[g2_1 n1] Work back up the tree, updating each node to reflect outcomes*)
let rec test_backPropogate () =
  ()

let test_iSMCTS () =
  let () = print_endline "testing ISMCTS: " in
  let deck = GameState.shuffle (GameState.getCardDeck [] 6) in
  let trump = Heart in
  let defender = {state= CPU 3; hand= []; name= "Foo"} in
  let attackers = [
    {state= CPU 3; hand= []; name= "Bar"};
    {state= CPU 3; hand= []; name= "Bob"};
    {state= CPU 3; hand= []; name= "Blarg"};
  ] in
  let table = [] in
  let active = List.hd attackers in
  let discard = [] in
  let winners = [] in
  let g = {
    deck; trump; defender; attackers; table; active; discard; winners
  } in
  let g1 = GameState.deal g in
  print_endline ("DECK: "^(printCardList g1.deck));
  print_endline ("DEFENDER: "^(printCardList g1.defender.hand));
  print_endline (printPlayerHands g1.attackers);
  let m = iSMCTS g1 5 in print_endline
    ("GRANDMASTERMOVE: "^(command_to_string m));
  ()

let test_med_defend () =
  let gs1 = {deck = [(Club, 8); (Spade, 13); (Spade, 12); (Spade, 10);
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
             table = [((Diamond, 14), None)];
             active =     {state = CPU 2;
                           hand = [(Heart, 11); (Spade, 14); (Diamond, 10);
                                   (Diamond, 8); (Diamond, 7); (Heart, 12)];
                           name = "Mary"};
             discard = [];
             winners = []} in
  let gs2 = {gs1 with deck = [];
                 discard = [(Club, 8); (Spade, 13); (Spade, 12); (Spade, 10);
                   (Diamond, 6); (Club, 9); (Club, 6); (Spade, 11);
                   (Spade, 9); (Diamond, 12); (Club, 7)]} in
  (* Early game, won't deflect because card is high *)
  assert (Medium.medium gs1 = Take);
  (* Late game, will deflect if possible *)
  assert (Medium.medium gs2 = Deflect ((Diamond, 14), (Spade, 14)));
  let gs3 = {gs1 with defender = {state = CPU 2;
                                  hand = [(Club, 11); (Diamond, 13); (Club, 12);
                                          (Heart, 10); (Heart, 7); (Heart, 6)];
                                  name = "Ivan"};
                      active = {state = CPU 2;
                                  hand = [(Club, 11); (Diamond, 13); (Club, 12);
                                          (Heart, 10); (Heart, 7); (Heart, 6)];
                                  name = "Ivan"};} in
  (* No moves *)
  assert (Medium.medium gs3 = Take);
  let gs4 = {gs3 with table = [((Diamond, 14), Some (Club, 9));
                               ((Heart, 9), None)]} in
  (* Defend one card, multiple on table, early game *)
  assert (Medium.medium gs4 = Defend ((Heart, 9), (Heart, 10)));
  let gs5 = {gs2 with defender = {state = CPU 2;
                                  hand = [(Club, 14); (Club, 13); (Club, 10);
                                          (Heart, 8); (Spade, 7); (Heart, 9)];
                                  name = "Jose"};
                      active = {state = CPU 2;
                                  hand = [(Club, 14); (Club, 13); (Club, 10);
                                          (Heart, 8); (Spade, 7); (Heart, 9)];
                                  name = "Jose"};
                      table = [((Diamond, 14), Some (Club, 9));
                               ((Club, 11), None);
                               ((Heart, 9), None);]} in
  (* Multiple defense choices, late game *)
  assert (Medium.medium gs5 = Defend ((Club, 11), (Club, 13)));
  ()


let test_med_attack () =
  let gs1 = {deck = [(Club, 8); (Spade, 13); (Spade, 12); (Spade, 10);
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
             winners = []} in
  (* No attacks yet *)
  assert (Medium.medium gs1 = Attack (Heart, 6));
  let gs2 = {gs1 with table = [((Club,8), Some (Club, 9));]} in
  (* No valid attacks *)
  assert (Medium.medium gs2 = Pass);
  let gs3 = {gs1 with table = [((Club,8), Some (Club, 13)); ((Heart, 13), None)]} in
  (* Pass rather than attacking with high cards early *)
  assert (Medium.medium gs3 = Pass);
  let gs4 = {gs3 with deck = []} in
  (* Always attack if possible late game *)
  assert (Medium.medium gs4 = Attack (Diamond, 13));
  let gs5 = {gs4 with table = [((Club,8), Some (Club, 13)); ((Heart, 13), None);
                              ((Diamond, 13), None); ((Diamond, 8), None);
                              ((Heart, 8), None); ((Spade, 8), None);]} in
  (* Don't attack if length of table is 6 *)
  assert (Medium.medium gs5 = Pass);
  ()

let run_ai_tests () =
  test_lowestValidDefOf ();
  test_easy_defend ();
  test_med_defend ();
  test_med_attack ();
  (*test_gameState_shuffle ();
  test_gameState_cloneAndRandomize ();
  test_gameState_doMove ();
  test_gameState_getMoves ();*)
  test_select ();
  test_expand ();
  test_simulate ();
  test_backPropogate ();
  (*test_iSMCTS ();*)
  print_endline "all AI tests pass";
  ()

let _ =
  run_ai_tests ()
