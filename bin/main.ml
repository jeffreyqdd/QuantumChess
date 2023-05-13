open Quantum
open Board
open Frontend
open Unix
open TerminalUtil

(*refresh rate of underlying logic*)
let fps = 60.

(*terminal mode*)
let current_mode = ref Normal

(*what the player has typed thus far*)
let text_buffer = ref ""

(*cursor coordinates, in the formal (col, line)*)
let cursor_coord = ref (0, 0)

(*https://stackoverflow.com/questions/4130048/recognizing-arrow-keys-with-stdin*)
let setup_terminal =
  let regular_mode = tcgetattr stdin in
  let consume_mode =
    let x = regular_mode in
    { x with c_icanon = false; c_vmin = 0; c_vtime = 0 }
  in
  at_exit (fun _ -> tcsetattr stdin TCSAFLUSH regular_mode);
  tcsetattr stdin TCSAFLUSH consume_mode;
  (*hide cursor
    https://stackoverflow.com/questions/58738953/how-to-hide-the-cursor-in-the-terminal-using-ocaml*)
  Printf.printf "\027[?25l%!"

type keyboard_signal =
  | Esc (*go into normal mode*)
  | Ins (*go into insertion mode*)
  | Submit (*submit user input*)
  | Pass (*nothing has happened*)
  | Play (*play signal*)
  | Quit (*quit signal*)
  | Help (*help signal*)

let process_string mode str =
  let rec match_codes code_lst =
    match mode with
    | Insertion -> handle_insertion code_lst
    | Normal -> handle_normal code_lst
  and handle_insertion lst =
    match lst with
    | c :: t when c = Ascii.escape -> Esc
    | c :: t when c = Ascii.newline -> Submit
    | c :: t when c = Ascii.backspace ->
        if String.length !text_buffer > 0 then
          text_buffer :=
            String.sub !text_buffer 0 (String.length !text_buffer - 1)
        else text_buffer := "";
        match_codes t
    (*all other insertion scenarios are considered input*)
    | c :: t ->
        text_buffer := !text_buffer ^ String.make 1 (Char.chr c);
        match_codes t
    | [] -> Pass
  and handle_normal lst =
    match lst with
    (*commands in normal mode*)
    | c :: t when c = Ascii.q -> Quit
    | c :: t when c = Ascii.a -> Help
    | c :: t when c = Ascii.p -> Play
    | c :: t when c = Ascii.i -> Ins
    | c :: t when c = Ascii.h ->
        (cursor_coord := Coords.(!cursor_coord + left));
        match_codes t
    | c :: t when c = Ascii.j ->
        (cursor_coord := Coords.(!cursor_coord + down));
        match_codes t
    | c :: t when c = Ascii.k ->
        (cursor_coord := Coords.(!cursor_coord + Coords.up));
        match_codes t
    | c :: t when c = Ascii.l ->
        (cursor_coord := Coords.(!cursor_coord + right));
        match_codes t
    | c :: t -> match_codes t
    | [] -> Pass
  in
  (*ensure bounds*)
  cursor_coord :=
    (min (max (fst !cursor_coord) 0) 7, min (max (snd !cursor_coord) 1) 8);
  let char_sequence =
    str |> String.to_seq |> List.of_seq |> List.map (fun x -> Char.code x)
  in
  match_codes char_sequence

let user_input_loop func =
  let flag = ref true in
  while !flag do
    let time_now = Unix.time () in
    let user_input = try read_line () with End_of_file -> "" in
    let parse_result = process_string !current_mode user_input in
    func flag parse_result;
    Render.command_bar !current_mode ~text:!text_buffer;
    Render.goto Coords.(command_text + (String.length !text_buffer, 0));
    (*take into consideration logic time into calcualting fps*)
    Unix.sleepf (max 0. ((1. /. fps) -. (Unix.time () -. time_now)))
  done

let handle_intro_page () =
  user_input_loop (fun flag parse_result ->
      match parse_result with
      | Esc -> current_mode := Normal
      | Play -> flag := false
      | Quit -> exit 0
      | _ -> ())

let board = ref (QFen.board_from_fen QFen.start)

let handle_chess_page () =
  let last_coord = ref (0, 0) in
  user_input_loop (fun flag parse_result ->
      (match parse_result with
      | Esc -> current_mode := Normal
      | Ins -> current_mode := Insertion
      | Quit -> flag := false
      | Submit -> text_buffer := ""
      | _ -> ());

      Render.background ();
      Render.draw Coords.board_start []
        (Frontend.string_of_board !board
           (Char.chr (fst !cursor_coord + Ascii.a))
           (snd !cursor_coord) ~render_highlight:(!current_mode = Normal));
      if Coords.(!last_coord <> !cursor_coord) then last_coord := !cursor_coord;
      (try
         Render.draw Coords.data_start []
           (Frontend.tile_info !board
              (Char.chr (fst !cursor_coord + Ascii.a))
              (8 - snd !cursor_coord))
       with Invalid_argument s -> ());

      () (* Render.goto *))

let main =
  (*title*)
  Render.clear ();
  Render.background ();
  Render.title ();
  Render.command_bar !current_mode;
  handle_intro_page ();

  (*game*)
  Render.clear ();
  Render.background ();
  handle_chess_page ()
