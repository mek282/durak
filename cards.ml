type suit = | Heart | Club | Spade | Diamond

type player_state = | Human | CPU of int

type card = suit * int

type deck = card list

type player = { state : player_state;
                hand : deck;
                name : string;
              }


(* Correspond to user inputs:
 * "Attack with [card]"
 * "Defend against [card] with [card]"
 * "Take"
 * "Pass"
 * "Deflect against [card] with [card]" *)
type command = | Attack of card | Defend of (card * card) | Take | Pass
               | Deflect of (card * card)

let print_suit s =
  match s with
  | Heart -> Printf.printf "Heart"
  | Diamond -> Printf.printf "Diamond"
  | Club -> Printf.printf "Club"
  | Spade -> Printf.printf "Spade"

let print_card c =
  match c with
  | (Heart, x) -> Printf.printf "\n%i of Hearts\n" x
  | (Diamond, x) -> Printf.printf "\n%i of Diamonds\n" x
  | (Club, x) -> Printf.printf "\n%i of Clubs\n" x
  | (Spade, x) -> Printf.printf "\n%i of Spades\n" x

let rec print_deck t clist =
  match clist with
  | [] -> Printf.printf "\nTRUMP: "; print_suit t
  | hd::tl -> (print_card hd; print_deck t tl)

let print_command command =
  match command with
  | Attack x      -> print_string "Attacked with ";
                     print_card x
  | Defend(x, y)  -> print_string "Defended ";
                     print_card x;
                     print_string " with ";
                     print_card y
  | Take          -> print_endline "Take"
  | Pass          -> print_endline "Pass"
  | Deflect(x, y) -> print_string "Deflected ";
                     print_card x;
                     print_string " with ";
                     print_card y

type state = { deck: deck;
               trump: suit;
               attackers: player list;
               defender: player;
               table: (card * card option) list;
               active: player;
               discard: deck;
               winners: player list;
               passed: player list;
              }