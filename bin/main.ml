open Game
open Deck
open Card
open Player

let rec remove_cards (lst1 : 'a list) (lst2 : 'a list) : 'a list =
  match lst2 with
  | [] -> []
  | h :: t -> remove_cards (List.filter (fun x -> x <> h) lst1) t

let count_number players =
  for i = 0 to Array.length players - 1 do
    print_endline
      ("Player " ^ string_of_int i ^ " has "
      ^ string_of_int (List.length (Player.player_hand players.(i)))
      ^ " cards left")
  done

let rec play_game (players : player array) (current : int) =
  print_endline
    ("Player " ^ string_of_int (current + 1) ^ "Please enter a valid command ");
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | s -> (
      try
        match Player.parse_move s with
        | Play a ->
            let h = remove_cards (player_hand players.(current)) a in
            players.(current) <-
              make_player h true (if List.length h = 0 then true else false)
        | Pass ->
            players.(current) <-
              make_player (player_hand players.(current)) false false;
            if Array.length players = current + 1 then play_game players 0
            else play_game players (current + 1)
        | Show ->
            print_endline
              ("your hand is " ^ show_hand (player_hand players.(current)));
            play_game players current
        | Count ->
            count_number players;
            play_game players current
      with Invalid ->
        print_endline "That command was invalid";
        play_game players current)

let start_game_helper (n : int) (deck : Deck.deck) : player array =
  let x = Array.make n (Player.make_player [] false false) in
  let y = Deck.deal deck n (n / 13) in
  x.(0) <- make_player y.(0) true false;
  for i = 1 to n do
    x.(i) <- make_player y.(i) false false
  done;
  x

let rec start_game (str : string) =
  try
    let n = int_of_string str in
    if n = 3 then
      let s = Deck.shuffle clean_deck in
      match Deck.get_cards s with
      | [] -> print_endline ""
      | h :: t ->
          print_endline ("The odd card out is " ^ card_string h);
          play_game (start_game_helper n (make_deck t)) 0
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
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to \n";
  print_endline
    "Please enter the number of players (2-4) you want to start a game with\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | a -> start_game a

let () = main ()
