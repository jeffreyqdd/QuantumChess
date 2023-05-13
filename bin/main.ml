open Quantum
open Board
open Frontend
open State
module IntMap = Map.Make (Int)

let _ =
  let board = ref (QFen.board_from_fen "r0/b1/8/8/8/8/8/8 - b - -") in
  let black_rook_id = (Board.piece_by_tile !board ('a', 7)).id in
  let black_bishop_id = (Board.piece_by_tile !board ('a', 6)).id in
  let print_rook () =
    black_rook_id |> Board.piece_by_id !board |> string_of_piece
    |> print_endline
  in

  (* TEST *)
  print_rook ();
  board :=
    black_rook_id |> Board.piece_by_id !board
    |> Board.remove_piece_tile !board ('a', 7);
  print_rook ();

  board :=
    Board.add_piece_tile !board ('a', 7)
      (black_rook_id |> Board.piece_by_id !board)
      20.0;
  board :=
    Board.add_piece_tile !board ('b', 7)
      (black_rook_id |> Board.piece_by_id !board)
      20.0;
  board :=
    Board.add_piece_tile !board ('c', 7)
      (black_rook_id |> Board.piece_by_id !board)
      20.0;
  board :=
    Board.add_piece_tile !board ('d', 7)
      (black_rook_id |> Board.piece_by_id !board)
      20.0;
  board :=
    Board.add_piece_tile !board ('e', 7)
      (black_rook_id |> Board.piece_by_id !board)
      20.0;
  print_rook ();
  (* board := Board.delete_piece !board (black_rook_id |> Board.piece_by_id
     !board); *)
  (* board := Board.remove_piece_tile !board ('a', 7) (black_rook_id |>
     Board.piece_by_id !board); board := Board.remove_piece_tile !board ('b', 7)
     (black_rook_id |> Board.piece_by_id !board); *)
  draw !board 'a' 0;
  print_endline " ";
  board := Measure.measurement !board ('a', 7);
  draw !board 'a' 0;
  print_rook ();
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
