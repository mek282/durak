(*** Types to be used in Durak ***)

type suit = | Heart | Club | Spade | Diamond
type player_state = | Human | CPU of int

type card = suit * int
type deck = card list
type player = { state : player_state;
                hand : deck
              }