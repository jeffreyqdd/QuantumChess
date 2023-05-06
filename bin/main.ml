open Quantum
open Board
open Frontend
open Unix

(*refresh rate of underlying logic*)
let fps = 0.5

(*terminal mode*)
let current_mode = ref Normal

(*cursor coordinates*)
let cursor_coord = ref (0, 0)

(*https://stackoverflow.com/questions/4130048/recognizing-arrow-keys-with-stdin*)
let setup_terminal =
  let regular_mode = tcgetattr stdin in
  let consume_mode =
    let x = regular_mode in
    { x with c_icanon = false; c_vmin = 0; c_vtime = 0 }
  in
  at_exit (fun _ -> tcsetattr stdin TCSAFLUSH regular_mode);
  tcsetattr stdin TCSAFLUSH consume_mode

type keyboard_signal =
  (*mode transitions*)
  | Esc
  | Ins

let process_string mode str =

  let single_match code = match mode, code with 
    | Insertion, (Char.code 's') ->  Esc
    (* | Normal, Ascii.i -> Ins *)
    (* match (mode, lst) with
    | Normal, "i" :: t -> EnterInsertion
    | Normal, "q" :: t -> Quit
    | Normal, "o" :: t -> Ok
    | Normal, "a" :: t -> Help
    | Normal, "h" :: t -> Left
    | Normal, "j" :: t -> Down
    | Normal, "k" :: t -> Up
    | Normal, "l" :: t -> Right
    | Normal, _ -> None
    | Insertion, [] -> Text ""
    | Insertion, "\\" :: t -> EnterNormal
    | Insertion, "\n" :: t -> Submit
    | Insertion, s :: t ->
        str_builder := !str_builder ^ s;
        helper t
  in *)
  let char_sequence = str |> String.to_seq |> List.of_seq |> List.map (fun x -> Char.code x) in
single_match char_sequence


let main =
  Render.clear ();
  Render.background ();
  Render.title ();
  Render.command_bar !current_mode;
  while true do
    let time_now = Unix.time () in
    let user_input = try read_line () with End_of_file -> "" in
    let parse_result = process_string !current_mode user_input in
    (*take into consideration logic time into calcualting fps*)
    Unix.sleepf (max 0. ((1. /. fps) -. (Unix.time () -. time_now)))
  done

(* let handle_cursor_thread tick_ms = let time_delay_sec = (tick_ms |>
   float_of_int) /. 1000. in while true do match !current_mode with | Normal ->
   ( let input = try read_line () with End_of_file -> "" in match input with |
   "" -> Thread.delay time_delay_sec | _ -> ()) | Insertion -> () done

   let _ = Terminal.set_mode Terminal.consume_mode; Render.render_title ();
   Render.render_command_bar !current_mode;

   while true do let x = try read_line () with End_of_file -> "" in if x <> ""
   then print_endline x done *)

(* Render.clear_screen (); Render.render_title (); Render.render_command_bar
   !current_mode *)

(* //let str = "8/r0r1:6:N2N3/8/8/8/k4:k4:k4:k4:k4:k4:k4:k4/8/8 - b - -" in *)
(* //let board = QFen.board_from_fen str in *)
(* //ANSITerminal.print_string [ ANSITerminal.red ] "Welcome to quantum
   chess!\n"; *)
(* //draw board *)
(*the idea is to set the cursor at a trash location so one can never see it!!*)
(*probably we will have a separate thread in ocaml deal with this!*)
(***)
