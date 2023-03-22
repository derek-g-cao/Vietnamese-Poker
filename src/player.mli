type player
(** The type representing a player in a game *)

(** The type [move] represents a move that a player can make during a game *)
type move =
  | Play of Card.t list
  | Pass
  | Show
  | Count

exception Invalid
(** Raised when a player move is invalid *)

val player_hand : player -> Card.t list
(** [player_hand a] takes in a player [a] and returns that player's hand *)

val in_round : player -> bool
(** [in_round a] takes in a player [a] and returns if that player is playing in
    the current round of play *)

val has_won : player -> bool
(** [has_won a] takes in a player [a] and returns if that player has already
    played all their cards *)

val make_player : Card.t list -> bool -> bool-> player
(** [make_player lst a b] takes in cards [lst] and booleans [a] and [b] to create a player with 
    those cards and booleans *)

val show_hand : Card.t list -> string
(** [show_hand lst] takes in cards and returns a string representation of those cards in order *)

val parse_move : string -> move
(** [parse_move a] takes in a player's input string as [a] and turns it into a
    valid move *)

