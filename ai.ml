open Cards

(*****************************************************************************)
(***********************************HELPERS***********************************)
(*****************************************************************************)

(*version of Pervasives.compare which ignores card suits*)
let cardCompare ((a,b):card) ((c,d):card) = compare b d


(* [sortHand a h t] outputs [h] sorted in increasing order, filtered by suit
* of [a] and [t] *)
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
                 then a::getUndefended tl
                 else getUndefended tl


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

  let medDefend (gameState:state) : command =
    failwith "TODO"

  let medAttack (gameState:state) : command =
    failwith "TODO"

  let medium (gameState:state) : command =
    if gameState.active = gameState.defender
      then mediumDefend gameState
    else if List.mem gameState.active gameState.attackers
      then mediumAttack gameState
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

let run_ai_tests () =
  test_lowestValidDefOf ();
  test_easy_defend ();
  print_endline "all AI tests pass";
  ()

let _ =
  run_ai_tests ()
