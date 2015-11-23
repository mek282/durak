type suit = | Heart | Club | Spade | Diamond

type player_state = | Human | CPU of int

type card = suit * int

type deck = card list

type player = { state : player_state;
                hand : deck;
                name : string;
              }


let print_suit s =
  match s with
  | Heart -> Printf.printf "Heart"
  | Diamond -> Printf.printf "Diamond"
  | Club -> Printf.printf "Club"
  | Spade -> Printf.printf "Spade"

let print_card c =
  match c with
  | (Heart, x) -> Printf.printf "\n%i of Hearts" x
  | (Diamond, x) -> Printf.printf "\n%i of Diamonds" x
  | (Club, x) -> Printf.printf "\n%i of Clubs" x
  | (Spade, x) -> Printf.printf "\n%i of Spades" x

let rec print_deck t clist =
  match clist with
  | [] -> Printf.printf "\nTRUMP: "; print_suit t
  | hd::tl -> (print_card hd; print_deck t tl)
