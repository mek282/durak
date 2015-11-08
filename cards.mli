(*** Types to be used in Durak ***)

type suit = | Heart | Club | Spade | Diamond
type player_state = | Human | CPU of int

type card = suit * int
type deck = card list
type player = { state : player_state;
                hand : deck
              }

(* Attackers index 0 represents primary attacker. Last index represents the
 * next player to be attacked.
 * Table is a list of pairs where the first element is an "attacking card" and
 * the second element is None or Some "defending card." *)
type state = { deck: deck;
               trump: suit;
               attackers: player list;
               defender: player;
               table: (card * card option) list;
               active: player;
               discard: deck;
               winners: player list;
              }

