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


(* Terminates everything and prints either a positive or negative message
 * depending on whether the player won or is a Durak. *)
let end_game (g : state) : unit =
  let _ =
    if List.exists (fun x -> x.state = Human) g.winners
    then print_endline "Congratulations! You didn't lose!"
    else print_endline "You lost--You're the durak!" in
  exit 0


(* Calls itself recursively to update the state in response to commands *)
let rec repl (g : state) (c : command) (message : string) : unit =
  print_state g;
  Printf.printf "starting repl\n%!";
  let (g',m,ended) = step g c in
  (if ended then end_game g' else ());
  Printf.printf "step done\n%!";
  let m' = message ^ " " ^ m in
  (* Gui.draw g'; *)
  Printf.printf "message done\n%!";
  Printf.printf "%b\n%!" (g'.active = g'.defender);
  let c' = parse_no_fail m' g' in
  Cards.print_command c';
  print_state g';
  if g.active.state = Human
    then (print_endline "first repl!!!"; repl g' c' "")
    else (print_endline "second repl!!!!!!"; repl g' c' m')
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

  let g1 = init_game_state "jane" [1;2;3] in
  assert (List.length (g1.deck) = 12);
  assert ((g1.defender).state = CPU 1);
  assert (List.length ((g1.defender).hand) = 6);
  assert ((g1.active).state = Human);
  assert ((g1.active).name = "jane");
  assert (List.length ((g1.active).hand) = 6);

  ()

let run_tests () =
  test_split ();
  test_parse_rank ();
  test_parse_suit ();
  test_parse_card ();
  test_parse ();
  print_endline "all tests pass";
  ()

let rec check_valid_difficulty () =
  let s = read_line () in
  match s with
  | "1" -> 1
  | "2" -> 2
  | "3" -> 3
  | _ -> print_endline "Invalid difficulty. Please type 1 for easy, 2 for medium, or 3 for hard.";
         check_valid_difficulty ()


let _ =
  title_screen ();
  print_endline "What is your name?";
  let name = read_line () in
  print_endline ("Hi " ^ name ^
    "! Choose the difficulty of your first opponent. Type 1 for easy, 2 for medium, or 3 for hard.");
  let d1 = check_valid_difficulty () in
  print_endline "Choose the difficulty of your second opponent. Type 1 for easy, 2 for medium, or 3 for hard.";
  let d2 = check_valid_difficulty () in
  print_endline "Choose the difficulty of your third opponent. Type 1 for easy, 2 for medium, or 3 for hard.";
  let d3 = check_valid_difficulty () in
  let gs = init_game_state name [d1; d2; d3] in
(*   Gui.draw gs;
  print_endline "starting fuck ";
  let com = read_line () in *)
  print_endline "starting initial command";
  let initial_command = parse_no_fail "" gs in
  print_endline "got innitial command!";
  repl gs initial_command "";
  run_tests ()
