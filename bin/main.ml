open Game
open Deck
open Card
open Player

let count_number players =
  for i = 0 to Array.length players - 1 do
    print_endline
      ("Player "
      ^ string_of_int (i + 1)
      ^ " has "
      ^ string_of_int (List.length (Player.player_hand players.(i)))
      ^ " cards left")
  done

let rec play_game state =
  print_endline
    ("Player "
    ^ string_of_int (State.current_player state + 1)
    ^ " Please enter a valid command ");
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | s -> (
      try
        match Player.parse_move s with
        | Play a -> (
            try
              let c = Rules.make_combo a in
              if Rules.valid_play (State.current_combo state) c then (
                let h =
                  Player.remove_cards
                    (State.players state).(State.current_player state)
                    a
                in
                (State.players state).(State.current_player state) <- h;
                play_game (State.change_combo state c |> State.change_player))
              else
                print_endline
                  "You are trying to play a combination that would be invalid \
                   on this board";
              play_game state
            with Rules.InvalidCombo ->
              print_endline "Invalid combo";
              play_game state)
        | Pass ->
            (State.players state).(State.current_player state) <-
              Player.pass (State.players state).(State.current_player state);
            play_game (State.change_player state)
        | Show ->
            print_endline
              ("your hand is "
              ^ show_hand
                  (player_hand
                     (State.players state).(State.current_player state)));
            play_game state
        | Count ->
            count_number (State.players state);
            play_game state
        | Quit ->
            print_endline "exiting the game. Thanks for playing nerdy nerd nerd";
            Stdlib.exit 0
      with Invalid ->
        print_endline "That command was invalid";
        play_game state)

let start_game_helper (n : int) (deck : Deck.deck) : State.t =
  let x = Array.make n (Player.make_player [] false false) in
  let y = Deck.deal deck n in
  for i = 0 to n - 1 do
    x.(i) <- make_player (List.sort Rules.compare y.(i)) true false
  done;
  State.start x 0

let rec start_game (str : string) =
  try
    let n = int_of_string str in
    if n = 3 then (
      let s = Deck.shuffle clean_deck in
      match Deck.get_cards s with
      | [] -> print_endline ""
      | h :: t ->
          print_endline ("The odd card out is " ^ card_string h);
          play_game (start_game_helper n (make_deck t)))
    else play_game (start_game_helper n (Deck.shuffle clean_deck))
  with Failure e -> (
    print_endline
      ("That was an invalid int" ^ e
     ^ ". Please enter a valid number of players (2-4) you want to start a \
        game with\n");
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | int -> start_game int)

let main () =
  let y = deal (shuffle clean_deck) 2 in
  let x = make_player y.(0) false false in
  print_endline (Player.show_hand (player_hand x));

  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to Vietnamese Poker \n";

  print_endline
    "Please enter the number of players (2-4) you want to start a game with\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | a -> start_game a

let () = main ()
