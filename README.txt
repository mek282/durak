DURAK 
Mary Kaminski (mek282) 
Ivan Zaitsev (iz44) 
Drew Samuels (drs354) 
Jose Castro (jec332) 

SETUP INSTRUCTIONS
-------------------------------------------------------------------------

To run the game navigate to a folder containing all .ml and 
.mli files. Compile by typing the following on the command line: 

cs3110 compile game.ml

Run by typing the following:

cs3110 run game.ml

The game works best full screen. All instructions and commands 
are outlined on the title screen.

1920 x 1080 is the recommended screen resolution.
If you cannot see everything on the first card screen without 
scrolling up, reducing the font size on the terminal may 
improve your user experience. Please go to Edit > Preferences. 
Size 8 works for most screens.

GAME INSTRUCTIONS
--------------------------------------------------------------------------
  Welcome to Durak. The object of the game is to get rid of all your cards
  before your opponents do the same. There are no winners, only one loser:
  the 'DURAK', or Idiot!

  You will receive a hand of six cards from a standard deck with all cards
  numbered five or below removed. Aces are high. A random suit will be chosen
  and known as the 'trump suit.'

  When it is your turn to attack the next player, choose a card from your hand
  and type 'attack with [value] of [suit]'.

  The defender may 'deflect' an attack by placing down a card of the
  same value: 'deflect [attack value] of [attack suit] with [deflect value] of
  [deflect suit]'. This transfers the attack to the next player. You may
  only deflect either an initial attack or a string of deflections.

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
  attacker may only attack with either a six or a seven.

  When a turn ends, i.e. all attacks have been defended, deflected, or the
  defender chooses to take, as long as there are cards in the deck all players
  with fewer than six cards will be dealt cards until their hands are back to six.

  Careful! Don't play all of your high cards too early. When the deck runs out,
  you'll wish you had them.