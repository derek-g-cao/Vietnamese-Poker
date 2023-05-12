type t
(** Represents the current state of a game *)

val current_combo : t -> Rules.t
(** [current_combo s] takes in a game state [s] and gives the combination on the
    board *)

val played_by : t -> int option
(** [played_by s] takes in a game state [s] and gives the player that last
    played something *)

val current_player : t -> int
(** [current_player s] takes in game state [s] and gives the player whose turn
    it is to play *)

val change_player : t -> t
(** [next_turn s] takes in game state [s] and changes the turn to the next
    player *)

val change_last_played : int -> t -> t
(** [change_last_played p s] takes in game state [s] and changes the last person
    that played a combo to be player [i] *)

val change_round : t -> t
(** [change_round s] takes in a game state [s] and determines if a new round
    should be starting i.e. current_player = last_played *)

val change_combo : t -> Rules.t -> t
(** [change_combo s c] takes in game state [s] and combo [c] and changes the
    current combination of [s] to [c] *)

val players : t -> Player.player array
(** [players s] takes in a game state [s] and gives a list of the current
    players in the game *)

val start : Player.player array -> int -> t
(** [start p] represents a game's starting state with players [p] *)
