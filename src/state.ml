type t = {
  players : Player.player array;
  current_combo : Rules.t;
  last_played : int option;
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
  let x =
    if s.current_player + 1 = Array.length s.players then 0
    else s.current_player + 1
  in
  {
    players = s.players;
    current_combo = s.current_combo;
    last_played = s.last_played;
    current_player = x;
  }

let restart_round p =
  for i = 0 to Array.length p - 1 do
    p.(i) <- Player.return_to_round p.(i)
  done;
  p

let change_round s =
  match s.last_played with
  | None -> s
  | Some p ->
      if p = s.current_player then
        {
          players = restart_round s.players;
          current_combo = Rules.Empty;
          last_played = None;
          current_player = s.current_player;
        }
      else s

let change_last_played p s =
  {
    players = s.players;
    current_combo = s.current_combo;
    last_played = Some p;
    current_player = s.current_player;
  }

let change_combo s c =
  {
    players = s.players;
    current_combo = c;
    last_played = s.last_played;
    current_player = s.current_player;
  }
