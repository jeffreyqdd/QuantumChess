open Quantum
open Board
open Frontend
open State
module IntMap = Map.Make (Int)

let _ =
  let board = ref (QFen.board_from_fen "r0/b1/k2/8/8/8/8/8 - b - -") in
  let black_rook_id = (Board.top_piece !board ('a', 7)).id in
  let black_bishop_id = (Board.top_piece !board ('a', 6)).id in
  let black_king_id = (Board.top_piece !board ('a', 5)).id in
  let print_piece id =
    id |> Board.piece !board |> string_of_piece |> print_endline
  in

  (* TEST *)
  board := Board.remove_piece_tile !board ('a', 7) black_rook_id;
  board := Board.remove_piece_tile !board ('a', 6) black_bishop_id;
  board := Board.remove_piece_tile !board ('a', 5) black_king_id;

  board := Board.add_piece_tile !board ('a', 7) black_rook_id 50.0;
  board := Board.add_piece_tile !board ('b', 7) black_rook_id 25.0;
  board := Board.add_piece_tile !board ('c', 7) black_rook_id 25.0;

  board := Board.add_piece_tile !board ('a', 6) black_bishop_id 50.0;
  board := Board.add_piece_tile !board ('b', 6) black_bishop_id 25.0;
  board := Board.add_piece_tile !board ('c', 6) black_bishop_id 25.0;

  board := Board.add_piece_tile !board ('a', 5) black_king_id 50.0;
  board := Board.add_piece_tile !board ('b', 5) black_king_id 25.0;
  board := Board.add_piece_tile !board ('c', 5) black_king_id 25.0;

  print_piece black_rook_id;
  print_piece black_bishop_id;
  print_piece black_king_id;
  (* board := Board.delete_piece !board (black_rook_id |> Board.piece
     !board); *)
  (* board := Board.remove_piece_tile !board ('a', 7) (black_rook_id |>
     Board.piece !board); board := Board.remove_piece_tile !board ('b', 7)
     (black_rook_id |> Board.piece !board); *)
  draw !board 'a' 0;
  print_endline " ";
  board := Measure.measurement !board ('b', 7);
  draw !board 'a' 0;
  print_piece black_rook_id;
  print_piece black_bishop_id;
  print_piece black_king_id;
  (* board := Board.add_piece_tile !board ('a', 7) black_rook 20.0; board :=
     Board.add_piece_tile !board ('b', 7) black_rook 20.0; board :=
     Board.add_piece_tile !board ('c', 7) black_rook 20.0; board :=
     Board.add_piece_tile !board ('d', 7) black_rook 20.0; board :=
     Board.add_piece_tile !board ('e', 7) black_rook 20.0; *)
  ANSITerminal.print_string [ ANSITerminal.red ] "Welcome to quantum chess!\n"
(* Measure *)
(* board := Board.delete_piece !board black_rook; *)
(* board := Board.remove_piece_tile !board ('a', 7) black_rook; board :=
   Board.remove_piece_tile !board ('b', 7) black_rook; *)
(* draw !board 'a' 0; *)
