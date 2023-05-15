open OUnit2
open Quantum
open Board
open State
open Util

let qfen_init_pieces_test (name : string) (qfen : string)
    (test_positions : (char * int * piece_type list) list) : test list =
  let board = QFen.board_from_fen qfen in
  let build_test b t =
    let file, rank, p = t in
    let test_name = String.make 1 file ^ string_of_int rank ^ " on " ^ name in
    test_name >:: fun _ ->
    assert_equal p
      (Board.tile b (file, rank) |> List.map (fun piece -> piece.piece_type))
      ~printer:(string_of_list pp_piece)
  in

  List.fold_left
    (fun acc position -> build_test board position :: acc)
    [] test_positions

let qfen_init_metadata_test (name : string) (qfen : string) (bq_castle : bool)
    (bk_castle : bool) (wq_castle : bool) (wk_castle : bool) (turn : color)
    (pipi : int list) : test list =
  let board = QFen.board_from_fen qfen in
  [
    ( name ^ " " ^ "turn" >:: fun _ ->
      assert_equal turn (Board.player_turn board) ~printer:pp_color );
  ]

let board_to_qfen_test (name : string) (qfen : string) =
  let board = QFen.board_from_fen qfen in
  let qfen2 = QFen.fen_from_board board in
  name >:: fun _ -> assert_equal qfen qfen2 ~printer:pp_string

let ensure_exn_from_qfen (name : string) (qfen : string) (e : exn) =
  name >:: fun _ -> assert_raises e (fun _ -> QFen.board_from_fen qfen)

let is_equal_test (name : string) (board1 : Board.t) (board2 : Board.t)
    (expected : bool) =
  name >:: fun _ ->
  assert_equal (Board.is_equal board1 board2) expected ~printer:string_of_bool

let player_turn_test (name : string) (board : Board.t) (expected : color) =
  name >:: fun _ ->
  assert_equal (Board.player_turn board) expected ~printer:pp_color

let tile_test (name : string) (board : Board.t) (square : coord)
    (expected : tile) =
  name >:: fun _ -> assert_equal (Board.tile board square) expected

let piece_test name board expected =
  name >:: fun _ -> assert_equal (Board.pieces board) expected

let pieces_test name board expected =
  name >:: fun _ -> assert_equal (Board.pieces board) expected

let top_piece_test name board square expected =
  name >:: fun _ -> assert_equal (Board.top_piece board square) expected

let piece_probability_test name board square piece expected =
  name >:: fun _ ->
  assert_equal (Board.piece_probability board square piece) expected

let tile_probability_test name board square expected =
  name >:: fun _ -> assert_equal (Board.tile_probability board square) expected

let set_piece_test name board piece1 piece2 expected =
  name >:: fun _ -> assert_equal (Board.set_piece board piece1 piece2) expected

let add_piece_tile_test name board square id probability expected =
  name >:: fun _ ->
  assert_equal (Board.add_piece_tile board square id probability) expected

let remove_piece_tile_test name board square id expected =
  name >:: fun _ ->
  assert_equal (Board.remove_piece_tile board square id) expected

let delete_piece_test name board piece expected =
  name >:: fun _ -> assert_equal (Board.delete_piece board piece) expected

let tests =
  List.flatten
    [
      qfen_init_pieces_test "Starting position" QFen.start
        [
          ('h', 7, [ { name = Rook; color = Black } ]);
          ('g', 7, [ { name = Knight; color = Black } ]);
          ('f', 7, [ { name = Bishop; color = Black } ]);
          ('e', 7, [ { name = King; color = Black } ]);
          ('d', 7, [ { name = Queen; color = Black } ]);
          ('c', 7, [ { name = Bishop; color = Black } ]);
          ('b', 7, [ { name = Knight; color = Black } ]);
          ('a', 7, [ { name = Rook; color = Black } ]);
          ('h', 6, [ { name = Pawn; color = Black } ]);
          ('g', 6, [ { name = Pawn; color = Black } ]);
          ('f', 6, [ { name = Pawn; color = Black } ]);
          ('e', 6, [ { name = Pawn; color = Black } ]);
          ('d', 6, [ { name = Pawn; color = Black } ]);
          ('c', 6, [ { name = Pawn; color = Black } ]);
          ('b', 6, [ { name = Pawn; color = Black } ]);
          ('a', 6, [ { name = Pawn; color = Black } ]);
          ('h', 0, [ { name = Rook; color = White } ]);
          ('g', 0, [ { name = Knight; color = White } ]);
          ('f', 0, [ { name = Bishop; color = White } ]);
          ('e', 0, [ { name = King; color = White } ]);
          ('d', 0, [ { name = Queen; color = White } ]);
          ('c', 0, [ { name = Bishop; color = White } ]);
          ('b', 0, [ { name = Knight; color = White } ]);
          ('a', 0, [ { name = Rook; color = White } ]);
          ('h', 1, [ { name = Pawn; color = White } ]);
          ('g', 1, [ { name = Pawn; color = White } ]);
          ('f', 1, [ { name = Pawn; color = White } ]);
          ('e', 1, [ { name = Pawn; color = White } ]);
          ('d', 1, [ { name = Pawn; color = White } ]);
          ('c', 1, [ { name = Pawn; color = White } ]);
          ('b', 1, [ { name = Pawn; color = White } ]);
          ('a', 1, [ { name = Pawn; color = White } ]);
          ('h', 5, []);
          ('g', 5, []);
          ('f', 5, []);
          ('e', 5, []);
          ('d', 5, []);
          ('c', 5, []);
          ('b', 5, []);
          ('a', 5, []);
          ('h', 4, []);
          ('g', 4, []);
          ('f', 4, []);
          ('e', 4, []);
          ('d', 4, []);
          ('c', 4, []);
          ('b', 4, []);
          ('a', 4, []);
          ('h', 3, []);
          ('g', 3, []);
          ('f', 3, []);
          ('e', 3, []);
          ('d', 3, []);
          ('c', 3, []);
          ('b', 3, []);
          ('a', 3, []);
          ('h', 2, []);
          ('g', 2, []);
          ('f', 2, []);
          ('e', 2, []);
          ('d', 2, []);
          ('c', 2, []);
          ('b', 2, []);
          ('a', 2, []);
        ];
      qfen_init_pieces_test "Empty" "8/8/8/8/8/8/8/8 - b - -" [ ('a', 7, []) ];
      qfen_init_metadata_test "Empty" "8/8/8/8/8/8/8/8 - w - -" true true true
        true White [];
      qfen_init_metadata_test "Empty" "8/8/8/8/8/8/8/8 - b - -" true true true
        true Black [];
    ]
  @ [
      board_to_qfen_test "Preserve start position" QFen.start;
      board_to_qfen_test "Preserve castling rights KQ"
        "8/8/8/8/8/8/8/8 - b KQ -";
      board_to_qfen_test "Preserve castling rights kq"
        "8/8/8/8/8/8/8/8 - b kq -";
      board_to_qfen_test "Preserve castling rights none"
        "8/8/8/8/8/8/8/8 - b - -";
      board_to_qfen_test "Preserve pipi"
        "8/8/8/1:p0:p1:p2:4/8/8/8/8 - b - 0:1:2:3";
      board_to_qfen_test "Preserve capture_attempts 1"
        "8/8/8/1:p0:p1:p2:4/8/8/8/8 1 b - -";
      board_to_qfen_test "Preserve capture_attempts 2"
        "8/8/8/1:p0:p1:p2:4/8/8/8/8 0:1:2 b - -";
      ensure_exn_from_qfen "Too few parts" "8/8/5:h0:2/8/8/8/8/8 - b - -"
        QFen.MalformedQFen;
      ensure_exn_from_qfen "Too many parts" "8/8/5:h0:2/8/8/8/8/8 - b - - - -"
        QFen.MalformedQFen;
      ensure_exn_from_qfen "Nonexistent piece" "8/8/5:h0:2/8/8/8/8/8 - b - -"
        QFen.MalformedQFen;
      ensure_exn_from_qfen "Missing piece" "8/8/5:r0:r:1/8/8/8/8/8 - b - -"
        QFen.MalformedQFen;
      is_equal_test "starting board is equal" Board.init Board.init true;
      player_turn_test "starting board is white player turn" Board.init White;
      tile_test "starting board is empty at b3" Board.init ('b', 3) [];
      top_piece_test "find top piece at a7" Board.init ('a', 7)
        (Board.tile Board.init ('a', 7) |> List.hd);
      piece_probability_test "probability of piece on starting board is 100%"
        Board.init ('a', 7)
        (Board.top_piece Board.init ('a', 7))
        100.;
      tile_probability_test "probability of tile a7 on starting board is 100%"
        Board.init ('a', 7) 100.;
    ]
