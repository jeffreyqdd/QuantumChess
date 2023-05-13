open OUnit2
open Quantum
open State
open Move

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

let string_of_piece_name name =
  match name with
  | Pawn -> "Pawn"
  | Rook -> "Rook"
  | Knight -> "Knight"
  | Bishop -> "Bishop"
  | Queen -> "Queen"
  | King -> "King"

let string_of_position position =
  Char.escaped position.file
  ^ string_of_int position.rank
  ^ " "
  ^ string_of_float position.probability

let string_of_list element lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ element h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ element h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let string_of_piece piece =
  string_of_piece_name piece.piece_type.name
  ^ " "
  ^ string_of_list string_of_position piece.superpositions

(* TESTS ARE BELOW *)

(** [measure_test name board square expected] tests [Measure.measure] *)
let measure_test (name : string) (board : Board.t) (square : coord)
    (expected : Board.t list) =
  name >:: fun _ ->
  let result =
    List.fold_left
      (fun acc_bool possible_result ->
        acc_bool
        || Board.is_equal (Measure.measurement board square) possible_result)
      false expected
  in
  assert_equal result true

let input1 =
  let board = ref (Board.QFen.board_from_fen "r0/8/8/8/8/8/8/8 - b - -") in
  let black_rook_id = (Board.piece_by_tile !board ('a', 7)).id in

  board :=
    black_rook_id |> Board.piece_by_id !board
    |> Board.remove_piece_tile !board ('a', 7);
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
  !board

let expected1 =
  let board = ref (Board.QFen.board_from_fen "r0/8/8/8/8/8/8/8 - b - -") in
  let black_rook_id = (Board.piece_by_tile !board ('a', 7)).id in

  board :=
    black_rook_id |> Board.piece_by_id !board
    |> Board.remove_piece_tile !board ('a', 7);
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
  !board

let tests =
  [ measure_test "Black rook and black bishop" input1 ('a', 6) [ input1 ] ]
