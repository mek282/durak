open Cards

(*****************************************************************************)
(***********************************HELPERS***********************************)
(*****************************************************************************)

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


let isValidAtt (inPlay:(card*card option) list) (att:card) : bool =
  failwith "TODO"

let isValidDef (attacking:card) (defending:card) : bool =
  failwith "TODO"


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


 module GameState = struct
  (*[GetNextPlayer g] returns the next player to move in gameState [g]*)
  let getNextPlayer =
    failwith "TODO"

  (*returns a new deck of cards*)
  let getCardDeck () =
    failwith "TODO"

  (*Deals hands to players [ps] in gameState [g]*)
  let deal =
    failwith "TODO"

  (*[Clone g] returns a clone of gameState [g]*)
  let clone =
    failwith "TODO"

  (*[CloneAndRandomize g p] returns a clone of the given gameState [g], after
   *randomizing the elements that are invisible to the given player [p]*)
  let cloneAndRandomize (g:state) (p:player) =
    failwith "TODO"

  (*DoMove c g] updates gameState [g] by executing command [c] *)
  let doMove =
    failwith "TODO"

  (*[GetMoves g] returns all possible moves given gameState g*)
  let getMoves =
    failwith "TODO"

  (*[GetResult g p] returns the result of gameState [g] from point of view of
   *player [p]*)
  let getResult =
    failwith "TODO"
end


module Node = struct
  (*[GetUntriedMoves lms] returns the elements of lms for which this node does
   *not have children**)
  let getUntriedMoves legalMoves =
    failwith "TODO"

  (*[UCBSelectChild lms d] uses the UC1 formula to select a child node, filtered
   *by the given list of legal moves [lms] and exploration coefficient [d]*)
  let uCBSelectChild =
    failwith "TODO"

  (*[AddChild m] adds a new child node to stateNode [n] for the move [m]*)
  let addChild =
    failwith "TODO"

  (*[Update n s] increment the visit count of node [n], increase win count of
   *[n] by the result of [GetResult p] for active player [p] of [s]*)
  let update =
    failwith "TODO"

  (*string representation of tree for debugging purposes*)
  let treeToSTring =
    failwith "TODO"
end



let iSMCTS =
  failwith "TODO"


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

  type gameStage = | Early | Late

  let getGameStage (gameState:state) : gameStage =
    let discarded = List.length (gameState.discard) in
    if discarded < 12
      then Early
    else Late

  (* [defAllAttacks h c g l] returns list of defenses in response to cards in
  * [c] from [h]. Not necessarily in the correct order. If a card cannot be
  * defended, no value will be added to the list. *)
  let rec defAllAttacks (hand:deck) (cs:card list) (gs:state) (lst:card list)
                        : card list =
    match cs with
    | [] -> lst
    | hd::tl -> (
      match lowestValidDefOf hand hd (gs.trump) with
      | None -> lst
      | Some c -> defAllAttacks (List.filter (fun x -> x <> c) hand) tl
                                gs (List.append lst [c]) )

  (* Medium AI defense calculator for early game*)
  let earlyDef (cs:card list) (gs:state) =
    (*One card to defend - deflect or defend if low, take if high*)
    if (List.length cs = 1)
      then if (snd (List.hd cs) >= 11 || fst (List.hd cs) = gs.trump)
             then Take
           else
             let at = List.hd cs in
             match lowestValidDefOf (gs.active).hand at (gs.trump) with
             | None -> Take
             | Some (a,b) -> if b = snd at && List.length gs.table = 1
                               then Deflect (at, (a,b))
                             else Defend (at, (a,b))
    (*Multiple cards on the table*)
    else let defList = defAllAttacks ((gs.active).hand) cs gs [] in
         if List.length defList = List.length cs
           then let is_high = fun x -> (fst x) = gs.trump || (snd x) > 10 in
             if (float_of_int (List.length (List.filter is_high defList)) /.
                float_of_int (List.length defList)) >= 0.5
               then Take (*Take if more than >= half are high or trump*)
             else let first = List.hd cs in
                  (match lowestValidDefOf (gs.active).hand first (gs.trump) with
                   | None -> Take
                   | Some c -> Defend (first, c)) (*Defend low cards if can*)
         else Take (*Take if not valid moves for all cards on table*)

  (* Medium AI defense calculator for late game*)
  let lateDef cs gs =
    (* if
    Easy.easyDefend gs
    then *)
    failwith "TODO"

  let medDefend (gameState:state) : command =
    (* let attacks = getUndefended (gameState.table) in
    match (getGameStage gameState) with
    | Early -> earlyDef attacks gameState
    | Late -> lateDef gameState *)
    failwith "TODO"

  let earlyAtt gs =
    failwith "TODO"


  let medAttack (gameState:state) : command =
    match (getGameStage gameState) with
    | Early -> failwith "TODO"
    | Late -> failwith "TODO"

  let medium (gameState:state) : command =
    if gameState.active = gameState.defender
      then medDefend gameState
    else if List.mem gameState.active gameState.attackers
      then medAttack gameState
    else failwith "error: AI player neither a defender nor an attacker"

end


module Hard = struct
  let hard (gameState:state) : command =
    failwith "TODO"
end


let response (gameState:state) : command =
  match gameState.active.state with
  | CPU 1 -> Easy.easy gameState
  | CPU 2 -> Medium.medium gameState
  | CPU 3 -> Hard.hard gameState
  | _ -> failwith "[response] error. Invalid active player_state"

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

let test_med_defend () =
  failwith "TODO"

let test_med_attack () =
  failwith "TODO"

let run_ai_tests () =
  test_lowestValidDefOf ();
  test_easy_defend ();
(*   test_med_defend ();
  test_med_attack (); *)
  print_endline "all AI tests pass";
  ()

let _ =
  run_ai_tests ()
