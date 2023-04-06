open Quantum
open Board
open Frontend

let rec string_of_file fin =
  try
    let x = input_line fin in
    x ^ "\n" ^ string_of_file fin
  with End_of_file -> ""

let background = string_of_file (open_in "assets/background.txt")
let title = string_of_file (open_in "assets/title.txt")

let command_bar_normal =
  string_of_file (open_in "assets/command_bar_normal.txt")

let command_bar_insertion =
  string_of_file (open_in "assets/command_bar_insertion.txt")

type mode =
  | Normal
  | Insertion

module Render = struct
  open ANSITerminal

  let clear_screen _ = ANSITerminal.erase ANSITerminal.Screen

  let render_background _ =
    ANSITerminal.set_cursor 1 1;
    ANSITerminal.print_string [] background

  let render_title _ =
    ANSITerminal.set_cursor 1 1;
    ANSITerminal.print_string [] title

  let render_command_bar mode =
    ANSITerminal.set_cursor 1 22;
    ANSITerminal.erase ANSITerminal.Eol;
    ANSITerminal.print_string []
      (if mode = Normal then command_bar_normal else command_bar_insertion)
end

(* let handle_introduction _ = (*render*) let all_lines = read_all_lines []
   (open_in "assets/title.txt") in AnsiTools.reset_screen (); List.iter (fun x
   -> print_endline x) all_lines; (*get input*) AnsiTools.reset_command_bar ();
   let x = read_line () in () *)
let current_mode = ref Insertion

open Unix

let terminfo = tcgetattr stdin

let newterminfo =
  at_exit (fun _ -> tcsetattr stdin TCSAFLUSH terminfo);
  (* reset stdin when you quit*)
  tcsetattr stdin TCSAFLUSH
    { terminfo with c_icanon = false; c_vmin = 0; c_vtime = 0 }

let _ = print_endline "meow"
(*while true do let x = try read_line () with End_of_file -> "" in if x <> ""
  then print_endline x done*)
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
