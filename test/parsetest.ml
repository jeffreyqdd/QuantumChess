open OUnit2
open Quantum
open Board
open State
open Command
open Util

let parse_test (name : string) (input : string) (expected_output : command) :
    test =
  name >:: fun _ ->
  assert_equal expected_output (parse input) ~printer:command_string

let tests =
  [
    parse_test "resign1" "     resign" Resign;
    parse_test "resign2" "  resign        " Resign;
    parse_test "draw1" "     draw" Draw;
    parse_test "draw2" "  draw     " Draw;
    parse_test "move1" " a0 4 a7"
      (Move
         {
           id = 4;
           start_tiles = (Some ('a', 0), None);
           end_tiles = (Some ('a', 7), None);
         });
    parse_test "move2" "      h0     40   h7   "
      (Move
         {
           id = 40;
           start_tiles = (Some ('h', 0), None);
           end_tiles = (Some ('h', 7), None);
         });
    parse_test "split1" " a0 4 a6 a7"
      (Move
         {
           id = 4;
           start_tiles = (Some ('a', 0), None);
           end_tiles = (Some ('a', 6), Some ('a', 7));
         });
    parse_test "split2" "      a0  4   a6  a7"
      (Move
         {
           id = 4;
           start_tiles = (Some ('a', 0), None);
           end_tiles = (Some ('a', 6), Some ('a', 7));
         });
    parse_test "merge1" " a0 g5 4 a7"
      (Move
         {
           id = 4;
           start_tiles = (Some ('a', 0), Some ('g', 5));
           end_tiles = (Some ('a', 7), None);
         });
    parse_test "merge2" "  h0 g5 4 a7 "
      (Move
         {
           id = 4;
           start_tiles = (Some ('h', 0), Some ('g', 5));
           end_tiles = (Some ('a', 7), None);
         });
    ( "parse exception empty" >:: fun _ ->
      assert_raises Empty (fun () -> parse "     ") );
    ( "parse exception malformed draw" >:: fun _ ->
      assert_raises Malformed (fun () -> parse " draw     funky") );
    ( "parse exception malformed move without id" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "  a1 a2 a4 ") );
    ( "parse exception malformed move incorrect order 1" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "  32 a2 a4 ") );
    ( "parse exception malformed move incorrect order 2" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "  a1 a2 32 ") );
    ( "parse exception malformed move too many inputs" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "  a1 a2 33 a4 a5 ") );
    ( "parse exception malformed move rank input 1 out of bounds" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "  a8 33 a5 ") );
    ( "parse exception malformed move rank input 1 out of bounds" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "  i7 33 a5 ") );
    ( "parse exception malformed move rank input 2 out of bounds" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "  a7 33 a8 ") );
    ( "parse exception malformed move rank input 2 out of bounds" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "  a7 33 i5 ") );
  ]
