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

let create_string_list (lst: 'a list) (f: 'a -> string) : string =
  String.concat "" (List.map f lst)

let draw_deck (deck: deck) : unit =
  let n = List.length deck in
  let num = if n < 10 then
              "0" ^ string_of_int n
            else
              string_of_int n
  in
  Printf.printf
"  _______
 |       |
 | Cards |
 | Left: |
 | %s    |
 |_______|\n%!" num

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

let draw_row (hand: deck): unit =
  let rank x = rank_to_string (snd x) in
  let suit x = suit_to_string (fst x) in
  let plist1 = create_string_list hand (fun x -> "  _______  ")  in
  let plist2 = create_string_list hand (fun x -> " |"^rank x^"      | " ) in
  let plist3 = create_string_list hand (fun x -> " |       | ") in
  let plist4 = create_string_list hand (fun x -> " |  "^suit x^"   | ") in
  let plist5 = create_string_list hand (fun x -> " |______"^rank x^"| ") in
  Printf.printf "%s\n%!" plist1;
  Printf.printf "%s\n%!" plist2;
  Printf.printf "%s\n%!" plist3;
  Printf.printf "%s\n%!" plist4;
  Printf.printf "%s\n%!" plist3;
  Printf.printf "%s\n%!" plist5

let rec draw_hand (hand: deck) : unit =
  match hand with
  | [] -> ()
  | h1::h2::h3::h4::h5::h6::t -> draw_row (h1::h2::h3::h4::h5::h6::[]);
                                 draw_hand  t
  | lst -> draw_row lst

let rec draw_table (cplist : (card * card option) list) : unit =
  match cplist with
  | [] -> ()
  | (c1, c2) :: t -> (match c2 with
                      | None -> draw_row [c1]
                      | Some c3 -> draw_row [c1; c3]);
                      draw_table t


(* =================================MAIN======================================*)

let draw (s: state) : unit =
  clear_screen ();
  Printf.printf "YOUR HAND\n%!";
  draw_hand s.active.hand;
  Printf.printf "\nTHE TABLE\n%!";
  draw_table s.table;
  Printf.printf "\nTHE DECK\n%!";
  draw_deck (s.deck);
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