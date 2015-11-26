open Cards

(* ================================HELPERS====================================*)
let clear_screen () = ignore (Sys.command "clear")

let suit_to_string (suit: suit) : string =
  match suit with
  | Heart   -> "Ht"
  | Club    -> "Cl"
  | Diamond -> "Di"
  | Spade   -> "Sp"

let rank_to_string (rank: int) : string =
  match rank with
  | 14 -> "A"
  | 13 -> "K"
  | 12 -> "Q"
  | 11 -> "J"
  | n  -> string_of_int n

let string_of_list (lst: 'a list) (f: 'a -> string) : string =
  String.concat "   " (List.map f lst)

let string_of_assoc_list (lst: ('a * 'a option) list) (f: 'a -> string)
  (g: 'a -> string) : string =
  let newlst = List.map (fun x -> match x with
                                  | (c1, Some c2) -> f c1
                                  | (c1, None)    -> g c1) lst in
  String.concat "   " newlst

let string_of_assoc_list1 (lst: ('a * 'a option) list) (f: 'a -> string)
  (g: 'a -> string) : string =
  let newlst = List.map (fun x -> match x with
                                  | (c1, Some c2) -> f c2
                                  | (c1, None)    -> g c1) lst in
  String.concat "   " newlst

let draw_deck (deck: deck) (trump: suit): unit =
  let n = List.length deck in
  let num = if n < 10 then
              "0" ^ string_of_int n
            else
              string_of_int n
  in
  let tstr = suit_to_string trump in
  Printf.printf
"\nTrump: %s
  _______
 |       |
 | Cards |
 | Left: |
 | %s    |
 |_______|\n%!" tstr num

let draw_card (card: card) : unit =
  let rank = rank_to_string (snd card) in
  let suit = suit_to_string (fst card) in
  Printf.printf
"  _______
 |%s      |
 |       |
 |  %s   |
 |       |
 |______%s|\n%!" rank suit rank

let draw_card_overlap (card1: card) (card2: card): unit =
  let rank1 = rank_to_string (snd card1) in
  let suit1 = suit_to_string (fst card1) in
  let rank2 = rank_to_string (snd card2) in
  let suit2 = suit_to_string (fst card2) in
  Printf.printf
"  _______
 |%s      |
 |       |
 |  %s __|____
 |    |%s      |
 |____|       |
      |  %s   |
      |       |
      |______%s|  \n%!" rank1 suit1 rank2 suit2 rank2

let draw_row (hand: deck): unit =
  let out = [] in
  let rank x = rank_to_string (snd x) in
  let suit x = suit_to_string (fst x) in
  let out = string_of_list hand (fun x -> " _______ ") :: out in
  let out = string_of_list hand (fun x -> "|"^rank x^"      |" ) :: out in
  let out = string_of_list hand (fun x -> "|       |") :: out in
  let out = string_of_list hand (fun x -> "|  "^suit x^"   |") :: out in
  let out = string_of_list hand (fun x -> "|       |") :: out in
  let out = string_of_list hand (fun x -> "|______"^rank x^"|") :: out in
  List.iter (fun x -> Printf.printf "%s\n%!" x) (List.rev out)

let rec draw_hand (hand: deck) : unit =
  match hand with
  | [] -> ()
  | h1::h2::h3::h4::h5::h6::t -> draw_row (h1::h2::h3::h4::h5::h6::[]);
                                 draw_hand  t
  | lst -> draw_row lst

let draw_table_row (cplist: (card * card option) list) : unit =
  let out = [] in
  let rank x = rank_to_string (snd x) in
  let suit x = suit_to_string (fst x) in
  let (list1, list2) = List.split cplist in
  let out = string_of_list list1 (fun x -> " _______      ")           :: out in
  let out = string_of_list list1 (fun x -> "|"^rank x^"      |     ")  :: out in
  let out = string_of_list list1 (fun x -> "|       |     ")           :: out in
  let out = string_of_assoc_list cplist (fun x -> "|  "^suit x^" __|____ ")
                                        (fun y -> "|  "^suit y^"   |     ")
                                        :: out in
  let out = string_of_assoc_list1 cplist (fun x -> "|    |"^rank x^"      |")
                                        (fun y -> "|       |     ")
                                        :: out in
  let out = string_of_assoc_list cplist (fun x -> "|____|       |")
                                        (fun y -> "|______"^rank y^"|     ")
                                        :: out in
  let out = string_of_assoc_list1 cplist (fun x -> "     |  "^ suit x^"   |")
                                        (fun y -> "              ")
                                        :: out in
  let out = string_of_assoc_list1 cplist (fun x -> "     |       |")
                                        (fun y -> "              ")
                                        :: out in
  let out = string_of_assoc_list1 cplist (fun x -> "     |______"^rank x^"|")
                                        (fun y -> "              ")
                                        :: out in
  List.iter (fun x -> Printf.printf "%s\n%!" x) (List.rev out)

let draw_table (cplist : (card * card option) list) : unit =
  draw_table_row cplist

let draw_opponent_row (hand: deck) : unit =
  let plist1 = String.concat "" (List.map (fun x -> "_ ") hand) in
  let plist2 = String.concat "" (List.map (fun x -> " |") hand) in
  let plist3 = String.concat "" (List.map (fun x -> "_|") hand) in
  Printf.printf " __%s\n%!" plist1;
  Printf.printf "|  %s\n%!" plist2;
  Printf.printf "|  %s\n%!" plist2;
  Printf.printf "|__%s\n%!" plist3

let draw_opponents (attackers: player list) (defender: player)
  (winners: player list) (active: player): unit =
  let players = defender :: attackers in
  let players_filtered = List.filter (fun x -> x = active) players in
  List.iter (fun x ->  Printf.printf "%s\n%!" x.name;
                      if List.mem x winners then
                        Printf.printf "Not an Idiot\n%!"
                      else
                        draw_opponent_row x.hand) players_filtered



(* =================================MAIN======================================*)

let draw (s: state) : unit =
  clear_screen ();
  Printf.printf "YOUR HAND\n%!";
  draw_hand s.active.hand;
  Printf.printf "\nOPPONENT'S HANDS\n%!";
  draw_opponents s.attackers s.defender s.winners s.active;
  Printf.printf "\nTHE TABLE\n%!";
  draw_table s.table;
  Printf.printf "\nTHE DECK\n%!";
  draw_deck s.deck s.trump;
  Printf.printf "\nWhat will you do?:\n%!"


(* ================================TESTING====================================*)

let ptest_draw_hand () : unit =
  draw_hand ([(Heart, 7); (Spade, 14)])

let ptest_draw_table () : unit =
  draw_table [((Heart,7), None); ((Heart,8), Some(Heart,9))]

let ptest_draw () : unit =
  let deck = [] in
  let trump = Heart in

  let hand1 = [(Heart, 7); (Diamond, 7);  (Club,14); (Spade, 14)] in
  let player1 = {state = Human; hand = hand1; name = "Zapdoz"} in

  let hand2 = [(Diamond, 6); (Club, 10); (Club, 12); (Spade,13); (Diamond, 14)] in
  let player2 = {state= CPU(1); hand = hand2; name = "Rawr"} in

  let attackers = [player2] in
  let defender = player1 in
  let table = [((Club, 6), Some(Club,7));
               ((Diamond,6), Some(Diamond,8));
               ((Club,8), None);
               ((Heart,8), None);
               ((Spade, 8), Some(Spade,9))] in

  let active = player1 in
  let discard = [] in
  let winners = [] in
  let state = {deck=deck; trump=trump; attackers=attackers; defender=defender;
               table=table; active=active; discard=discard; winners=winners} in

  draw state

let run_tests = (* ptest_draw_hand ();
                ptest_draw_table (); *)
                ptest_draw ()