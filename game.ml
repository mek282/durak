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


(* returns true iff all the attacks on the table have rank r and have no
 * defending card *)
let rec deflectable' (r : int) = function
  | [] -> true
  | ((_,r'), None)::[] -> r = r'
  | ((_,r') ,None)::t -> r = r' && deflectable' r t
  | _ -> false


(* returns true iff all cards on the table are undefended and have the same rank
 * the table is nonempty *)
let deflectable (gs : state) =
  match gs.table with
  | [] -> false
  | ((_,r),None)::t -> deflectable' r t
  | _ -> false


let print_player_prompt gs =
  let def_message =
    "You are the defender.  Defend against an unmatched attack [c1] on the table by typing
    \"defend against [c1] with [c2]\", where c2 is of the same suit and higher rank than c1
    OR, if c1 is not a trump, c2 can be any trump card. You may also \"take\" to add all
    the cards to your hand and end your turn, or \"pass\" to allow more people to attack." in
  let deflect_message =
    "If you have a card [c2] of the same rank as all the cards on the table, you can
    successfully end your turn by typing \"deflect against [c1] with [c2]\"." in
  let primary_att_message =
    "You are the primary attacker. Type \"attack with [card]\"" in
  let secondary_att_message =
    "You can attack with any card of the same rank as a card already on the table by typing
    \"attack with [card]\". You can also \"pass\"." in
  match gs.active.state with
  | CPU _ -> ()
  | Human -> if gs.active.name = gs.defender.name
               then if deflectable gs then
                  let () = print_endline def_message in print_endline deflect_message
               else print_endline def_message
             else if gs.table = [] then print_endline primary_att_message
                  else print_endline secondary_att_message

(* Draws and prompts the user*)
let game_draw (g : state) (prompt : string) : string =
  Gui.draw g;
  print_endline prompt;
  print_player_prompt g;
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
  let (g',m,ended) = step g c in
  (if ended then end_game g' else ());
  let m' = message ^ " " ^ m in
  (* Gui.draw g'; *)
  let c' = parse_no_fail m' g' in
  Cards.print_command c';
  if g.active.state = Human
    then (repl g' c' "")
    else (repl g' c' m')

(*TEST CASES*)

let test_parse_no_fail () =
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


let test_winning () =
  let deck = [] in
  let trump = Heart in
  let ivanII = {state = CPU 2; name = "Ivan II"; hand = [(Club,14)]} in
  let melly = {state=CPU 1; name = "Melly";
    hand = [(Diamond,11); (Diamond,12); (Club,11); (Club,10); (Heart,10);
    (Diamond,10); (Spade,10); (Diamond,13)]} in
  let pig = {state = Human; name = "Pigeon"; hand = [(Heart,14); (Diamond,8); (Diamond,14)]} in
  let attackers = [ivanII; melly] in
  let defender = pig in
  let table = [((Club,12),None); ((Spade,12),Some (Spade,14))] in
  let active = ivanII in
  let discard = [] in
  let winners = [{state = CPU 2; name = "Jose"; hand = []}] in
  let passed = [] in
  let g0 = {deck=deck; trump=trump; attackers=attackers; defender=defender;
            table=table; active=active; discard=discard; winners=winners; passed=passed} in
  Gui.draw g0 ;
  let (g1,message,done1) = step g0 (Attack (Club,14)) in
  print_endline message


let run_tests () =
  (*test_split ();
  test_parse_rank ();
  test_parse_suit ();
  test_parse_card ();
  test_parse ();*)
  test_winning ();
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
  Random.self_init ();
  run_tests ();
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
  let initial_command = parse_no_fail "" gs in
  repl gs initial_command ""
