open Cards

type command = | Attack of card | Defend of (card * card) | Take | Pass
               | Deflect of card

val init_deck : () -> (suit, deck)
val init_game_state : () -> state
val parse : string -> command
