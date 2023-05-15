open OUnit2
open Quantum
open Board
open State
open Frontend
open Move
open Command
module IntMap = Map.Make (Int)

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

let board = ref (QFen.board_from_fen QFen.start)

(** [move_test_suite name board square expected] tests [Move.move] and ensures
    it doesn't error *)
let move_test_suite (name : string) (test : unit) =
  name >:: fun _ ->
  let no_fail =
    try
      let _ = test in
      true
    with _ -> false
  in
  assert_equal no_fail true ~printer:string_of_bool

let test1 =
  let black_pawn_id1 = (Board.top_piece !board ('a', 1)).id in
  let black_pawn_id2 = (Board.top_piece !board ('b', 1)).id in
  (* let b2_probability = Board.tile_probability !board ('b', 2) in *)
  let black_pawn_id3 = (Board.top_piece !board ('c', 1)).id in
<<<<<<< HEAD
  (*print_endline (string_of_int black_pawn_id3); *)
  let black_pawn_id4 = (Board.top_piece !board ('d', 1)).id in
=======
  (* let black_pawn_id4 = (Board.top_piece !board ('d', 1)).id in *)
>>>>>>> origin/move
  let black_pawn_id5 = (Board.top_piece !board ('e', 1)).id in
  (* let black_pawn_id6 = (Board.top_piece !board ('f', 1)).id in *)
  (* let black_pawn_id7 = (Board.top_piece !board ('g', 1)).id in *)
  (* let black_pawn_id8 = (Board.top_piece !board ('h', 1)).id in *)
  let black_rook_id1 = (Board.top_piece !board ('a', 0)).id in
  let black_bishop_id1 = (Board.top_piece !board ('c', 0)).id in
  let black_knight_id1 = (Board.top_piece !board ('g', 0)).id in
  let black_queen_id = (Board.top_piece !board ('d', 0)).id in
  let white_pawn_id1 = (Board.top_piece !board ('a', 6)).id in
  let white_pawn_id2 = (Board.top_piece !board ('b', 6)).id in
  let white_knight_id2 = (Board.top_piece !board ('g', 7)).id in

  (* let print_piece id = id |> Board.piece !board |> string_of_piece |>
     print_endline in *)

  (* let print_tile_probability tile = string_of_float tile |> print_endline
     in *)
  let move_pawnb_1 =
    {
      id = black_pawn_id1;
      start_tiles = (Some ('a', 1), None);
      end_tiles = (Some ('a', 2), None);
    }
  in
  let move_pawnb_1_2 =
    {
      id = black_pawn_id1;
      start_tiles = (Some ('a', 2), None);
      end_tiles = (Some ('a', 3), None);
    }
  in
  let move_pawnb_2 =
    {
      id = black_pawn_id2;
      start_tiles = (Some ('b', 1), None);
      end_tiles = (Some ('b', 3), None);
    }
  in
  let move_pawnb_3 =
    {
      id = black_pawn_id3;
      start_tiles = (Some ('c', 1), None);
      end_tiles = (Some ('c', 2), None);
    }
  in
  let move_pawnb_5 =
    {
      id = black_pawn_id5;
      start_tiles = (Some ('e', 1), None);
      end_tiles = (Some ('e', 3), None);
    }
  in
  let move_rook_1 =
    {
      id = black_rook_id1;
      start_tiles = (Some ('a', 0), None);
      end_tiles = (Some ('a', 1), None);
    }
  in
  let move_pawnw_1 =
    {
      id = white_pawn_id1;
      start_tiles = (Some ('a', 6), None);
      end_tiles = (Some ('a', 5), None);
    }
  in
  let move_pawnw_2 =
    {
      id = white_pawn_id2;
      start_tiles = (Some ('b', 6), None);
      end_tiles = (Some ('b', 4), None);
    }
  in
  let move_knightb_1 =
    {
      id = black_knight_id1;
      start_tiles = (Some ('g', 0), None);
      end_tiles = (Some ('h', 2), None);
    }
  in
  let move_bishopb_1 =
    {
      id = black_bishop_id1;
      start_tiles = (Some ('c', 0), None);
      end_tiles = (Some ('a', 2), None);
    }
  in
  let move_queen_b =
    {
      id = black_queen_id;
      start_tiles = (Some ('d', 0), None);
      end_tiles = (Some ('h', 4), None);
    }
  in
  let move_knightw_1 =
    {
      id = white_knight_id2;
      start_tiles = (Some ('g', 7), None);
      end_tiles = (Some ('h', 5), None);
    }
  in
  let queen_b_take =
    {
      id = black_queen_id;
      start_tiles = (Some ('h', 4), None);
      end_tiles = (Some ('h', 5), None);
    }
  in
  let queen_b_take_2 =
    {
      id = black_queen_id;
      start_tiles = (Some ('h', 5), None);
      end_tiles = (Some ('g', 6), None);
    }
  in
  let queen_b_take_3 =
    {
      id = black_queen_id;
      start_tiles = (Some ('g', 6), None);
      end_tiles = (Some ('f', 7), None);
    }
  in
  let queen_b_take_4 =
    {
      id = black_queen_id;
      start_tiles = (Some ('f', 7), None);
      end_tiles = (Some ('e', 7), None);
    }
  in
  let split_knightb_1 =
    {
      id = black_knight_id1;
      start_tiles = (Some ('h', 2), None);
      end_tiles = (Some ('g', 0), Some ('g', 4));
    }
  in
  let merge_knight_b1 =
    {
      id = black_knight_id1;
      start_tiles = (Some ('g', 0), Some ('g', 4));
      end_tiles = (Some ('h', 2), None);
    }
  in
  board := move !board move_pawnb_1;
  board := move !board move_pawnb_3;
  board := move !board move_pawnb_2;
  board := move !board move_rook_1;
  board := move !board move_pawnw_1;
  board := move !board move_pawnw_2;
  board := move !board move_knightb_1;
  board := move !board move_pawnb_1_2;
  board := move !board move_bishopb_1;
  board := move !board move_pawnb_5;
  board := move !board move_queen_b;
  board := move !board move_knightw_1;
  (* print_piece black_pawn_id1; print_piece black_pawn_id2; print_piece
     black_pawn_id3; print_piece black_rook_id1; print_piece white_pawn_id1;
     print_piece white_pawn_id2; print_piece black_knight_id1; *)
  draw !board 'a' 4

let tests = [ move_test_suite "Movement sequence does not error" test1 ]
