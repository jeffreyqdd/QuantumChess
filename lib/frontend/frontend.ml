open State
open Board

let ranks = [ 1; 2; 3; 4; 5; 6; 7; 8 ]
let files = [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h' ]

(** [piece_to_string p c] is a Unicode character representing piece [p] with
    color [c] *)
let piece_to_string (p : State.piece_name) (c : State.color) : string =
  match (c, p) with
  | White, King -> "\u{2654}"
  | White, Queen -> "\u{2655}"
  | White, Rook -> "\u{2656}"
  | White, Bishop -> "\u{2657}"
  | White, Knight -> "\u{2658}"
  | White, Pawn -> "\u{2659}"
  | Black, King -> "\u{265A}"
  | Black, Queen -> "\u{265B}"
  | Black, Rook -> "\u{265C}"
  | Black, Bishop -> "\u{265D}"
  | Black, Knight -> "\u{265E}"
  | Black, Pawn -> "\u{265F}"

(* NOT NEEDED IF SMALL BOARD, prints line separating files *)
(* let print_hline () = print_char '+'; for i = 1 to 8 do print_string "---+"
   done *)

(** [tile board file rank] is the tile represented by [file] and [rank] on some
    [board] *)
let tile (board : Board.t) (file : char) (rank : int) : string =
  match Board.tile board file rank with
  | [] -> "."
  | h :: t -> piece_to_string h.name h.color

(** [print_line board rank] prints the line in [board] represented by [rank] *)
let print_line (board : Board.t) (rank : int) : unit =
  files |> List.iter (fun file -> print_string (tile board file rank))

let draw board =
  (* clear screen *)
  let _ = Sys.command "clear" in
  ranks |> List.iter (fun rank -> print_line board rank)

(* tile board file rank -> returns the specified tile *)
