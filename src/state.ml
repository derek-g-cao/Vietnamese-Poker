type t = {
  players : Player.player array;
  current_combo : Rules.t;
  last_played : Player.player option;
  current_player : int;
}

let start p i =
  {
    players = p;
    current_combo = Rules.Empty;
    last_played = None;
    current_player = i;
  }

let current_combo s = s.current_combo
let played_by s = s.last_played
let current_player s = s.current_player
let players s = s.players

let change_player s =
  if s.current_player + 1 = Array.length s.players then
    {
      players = s.players;
      current_combo = s.current_combo;
      last_played = s.last_played;
      current_player = 0;
    }
  else
    {
      players = s.players;
      current_combo = s.current_combo;
      last_played = s.last_played;
      current_player = s.current_player + 1;
    }

let change_combo s c =
  {
    players = s.players;
    current_combo = c;
    last_played = s.last_played;
    current_player = s.current_player;
  }
