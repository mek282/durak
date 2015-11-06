


type suit = | Heart | Club | Spade | Diamond
type player_state = | Human | CPU of int

type card = suit * int
type deck = card list
type player = {state : player_state; hand : deck}

(* attackers index 0 primary attacker index lst next to be attacked *)
type state = {deck: deck; trump: suit; attackers: player list; defender: player; table: (card * card option) list}

