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
  let f x = element x in
  "[" ^ (List.map f lst |> String.concat "; ") ^ "]"

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
  assert_equal result true ~printer:string_of_bool

(** [measure_test_fail name board square expected] tests [Measure.measure] and
    ensures it doesn't error *)
let measure_test_fail (name : string) (board : Board.t) (square : coord) =
  name >:: fun _ ->
  let no_fail =
    try
      let _ = Measure.measurement board square in
      true
    with _ -> false
  in
  assert_equal no_fail true ~printer:string_of_bool

(* Test 1 *)
let input1 =
  let board = ref (Board.QFen.board_from_fen "r0/8/8/8/8/8/8/8 - b - -") in
  let black_rook_id = (Board.top_piece !board ('a', 7)).id in

  board := Board.remove_piece_tile !board ('a', 7) black_rook_id;
  board := Board.add_piece_tile !board ('a', 7) black_rook_id 20.0;
  board := Board.add_piece_tile !board ('b', 7) black_rook_id 20.0;
  board := Board.add_piece_tile !board ('c', 7) black_rook_id 20.0;
  board := Board.add_piece_tile !board ('d', 7) black_rook_id 20.0;
  board := Board.add_piece_tile !board ('e', 7) black_rook_id 20.0;
  !board

let expected1_1 =
  let board = ref (Board.QFen.board_from_fen "r0/8/8/8/8/8/8/8 - b - -") in
  let black_rook_id = (Board.top_piece !board ('a', 7)).id in

  board := Board.remove_piece_tile !board ('a', 7) black_rook_id;
  board := Board.add_piece_tile !board ('a', 7) black_rook_id 100.0;
  !board

let expected1_2 =
  let board = ref (Board.QFen.board_from_fen "r0/8/8/8/8/8/8/8 - b - -") in
  let black_rook_id = (Board.top_piece !board ('a', 7)).id in

  board := Board.remove_piece_tile !board ('a', 7) black_rook_id;
  board := Board.add_piece_tile !board ('b', 7) black_rook_id 25.0;
  board := Board.add_piece_tile !board ('c', 7) black_rook_id 25.0;
  board := Board.add_piece_tile !board ('d', 7) black_rook_id 25.0;
  board := Board.add_piece_tile !board ('e', 7) black_rook_id 25.0;
  !board

(* Test 2 *)
let input2 =
  let board = ref (Board.QFen.board_from_fen "r0/b1/k2/8/8/8/8/8 - b - -") in
  let black_rook_id = (Board.top_piece !board ('a', 7)).id in
  let black_bishop_id = (Board.top_piece !board ('a', 6)).id in
  let black_king_id = (Board.top_piece !board ('a', 5)).id in

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
  !board

let expected2_1 =
  let board = ref (Board.QFen.board_from_fen "r0/b1/k2/8/8/8/8/8 - b - -") in
  let black_rook_id = (Board.top_piece !board ('a', 7)).id in
  let black_bishop_id = (Board.top_piece !board ('a', 6)).id in
  let black_king_id = (Board.top_piece !board ('a', 5)).id in

  board := Board.remove_piece_tile !board ('a', 7) black_rook_id;
  board := Board.remove_piece_tile !board ('a', 6) black_bishop_id;
  board := Board.remove_piece_tile !board ('a', 5) black_king_id;

  board := Board.add_piece_tile !board ('b', 7) black_rook_id 100.0;

  board := Board.add_piece_tile !board ('a', 6) black_bishop_id 50.0;
  board := Board.add_piece_tile !board ('b', 6) black_bishop_id 25.0;
  board := Board.add_piece_tile !board ('c', 6) black_bishop_id 25.0;

  board := Board.add_piece_tile !board ('a', 5) black_king_id 50.0;
  board := Board.add_piece_tile !board ('b', 5) black_king_id 25.0;
  board := Board.add_piece_tile !board ('c', 5) black_king_id 25.0;
  !board

let expected2_2 =
  let board = ref (Board.QFen.board_from_fen "r0/b1/k2/8/8/8/8/8 - b - -") in
  let black_rook_id = (Board.top_piece !board ('a', 7)).id in
  let black_bishop_id = (Board.top_piece !board ('a', 6)).id in
  let black_king_id = (Board.top_piece !board ('a', 5)).id in

  board := Board.remove_piece_tile !board ('a', 7) black_rook_id;
  board := Board.remove_piece_tile !board ('a', 6) black_bishop_id;
  board := Board.remove_piece_tile !board ('a', 5) black_king_id;

  board := Board.add_piece_tile !board ('a', 7) black_rook_id 62.5;
  board := Board.add_piece_tile !board ('c', 7) black_rook_id 37.5;

  board := Board.add_piece_tile !board ('a', 6) black_bishop_id 50.0;
  board := Board.add_piece_tile !board ('b', 6) black_bishop_id 25.0;
  board := Board.add_piece_tile !board ('c', 6) black_bishop_id 25.0;

  board := Board.add_piece_tile !board ('a', 5) black_king_id 50.0;
  board := Board.add_piece_tile !board ('b', 5) black_king_id 25.0;
  board := Board.add_piece_tile !board ('c', 5) black_king_id 25.0;
  !board

let input3 =
  let board = ref (Board.QFen.board_from_fen "r0/b1/k2/8/8/8/8/8 - b - -") in
  let black_rook_id = (Board.top_piece !board ('a', 7)).id in
  let black_bishop_id = (Board.top_piece !board ('a', 6)).id in
  let black_king_id = (Board.top_piece !board ('a', 5)).id in
  board := Board.remove_piece_tile !board ('a', 7) black_rook_id;
  board := Board.remove_piece_tile !board ('a', 6) black_bishop_id;
  board := Board.remove_piece_tile !board ('a', 5) black_king_id;

  board := Board.add_piece_tile !board ('a', 7) black_rook_id 50.0;
  board := Board.add_piece_tile !board ('b', 7) black_rook_id 25.0;
  board := Board.add_piece_tile !board ('c', 7) black_rook_id 25.0;

  board := Board.add_piece_tile !board ('a', 7) black_bishop_id 50.0;
  board := Board.add_piece_tile !board ('b', 6) black_bishop_id 25.0;
  board := Board.add_piece_tile !board ('c', 6) black_bishop_id 25.0;

  board := Board.add_piece_tile !board ('a', 5) black_king_id 50.0;
  board := Board.add_piece_tile !board ('b', 5) black_king_id 25.0;
  board := Board.add_piece_tile !board ('c', 5) black_king_id 25.0;
  !board

let input4 =
  let board = ref (Board.QFen.board_from_fen "r0/b1/k2/8/8/8/8/8 - b - -") in
  let black_rook_id = (Board.top_piece !board ('a', 7)).id in
  let black_bishop_id = (Board.top_piece !board ('a', 6)).id in
  let black_king_id = (Board.top_piece !board ('a', 5)).id in
  board := Board.remove_piece_tile !board ('a', 7) black_rook_id;
  board := Board.remove_piece_tile !board ('a', 6) black_bishop_id;
  board := Board.remove_piece_tile !board ('a', 5) black_king_id;

  board := Board.add_piece_tile !board ('a', 7) black_rook_id 50.0;
  board := Board.add_piece_tile !board ('b', 7) black_rook_id 25.0;
  board := Board.add_piece_tile !board ('c', 7) black_rook_id 25.0;

  board := Board.add_piece_tile !board ('a', 7) black_bishop_id 50.0;
  board := Board.add_piece_tile !board ('b', 7) black_bishop_id 25.0;
  board := Board.add_piece_tile !board ('c', 6) black_bishop_id 25.0;

  board := Board.add_piece_tile !board ('a', 5) black_king_id 50.0;
  board := Board.add_piece_tile !board ('b', 7) black_king_id 50.0;
  !board

let tests =
  [
    measure_test
      "Black rook on a7/b7/c7/d7/e7 is measured to either be on a7 only with \
       100% probability or b7/c7/d7/e7 with 25% probability each"
      input1 ('a', 7)
      [ expected1_1; expected1_2 ];
    measure_test
      "Black rook on a7/b7/c7, black bishop on a6/b6/c6, black king on \
       a5/b5/c5 is measured to either be a rook on b7 only with 100% \
       probability or a rook on a7:62.5 and b7:37.5 probability each"
      input2 ('b', 7)
      [ expected2_1; expected2_2 ];
    measure_test_fail "More complex probabilities" input3 ('b', 7);
    measure_test_fail "Even more complex probabilities" input4 ('b', 7);
  ]
