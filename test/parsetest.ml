open OUnit2
open Quantum
open Board
open State
open Command

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(** [string_of_coord c] pretty-prints coordinate object [c]. *)
let string_of_coord c =
  match c with
  | a, b -> String.make 1 a ^ string_of_int b

(** [command_string c] pretty-prints command object [c]. *)
let command_string c =
  match c with
  | Move s -> (
      match (s.start_tiles, s.end_tiles) with
      | (Some a, None), (Some c, None) ->
          string_of_coord a ^ " " ^ string_of_coord c
      | (Some a, Some b), (Some c, None) ->
          string_of_coord a ^ " " ^ string_of_coord b ^ " to "
          ^ string_of_coord c
      | (Some a, None), (Some c, Some d) ->
          string_of_coord a ^ " to " ^ string_of_coord c ^ " "
          ^ string_of_coord d
      | _, _ -> " ")
  | Draw -> pp_string "draw"
  | Resign -> pp_string "resign"

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
    parse_test "move1" " a1 4 a8"
      (Move
         {
           id = 4;
           start_tiles = (Some ('a', 1), None);
           end_tiles = (Some ('a', 8), None);
         });
    parse_test "move2" "      h1     40   h8   "
      (Move
         {
           id = 40;
           start_tiles = (Some ('h', 1), None);
           end_tiles = (Some ('h', 8), None);
         });
    parse_test "split1" " a1 4 a7 a8"
      (Move
         {
           id = 4;
           start_tiles = (Some ('a', 1), None);
           end_tiles = (Some ('a', 7), Some ('a', 8));
         });
    parse_test "split2" "      a1  4   a7  a8"
      (Move
         {
           id = 4;
           start_tiles = (Some ('a', 1), None);
           end_tiles = (Some ('a', 7), Some ('a', 8));
         });
    parse_test "merge1" " a1 g6 4 a8"
      (Move
         {
           id = 4;
           start_tiles = (Some ('a', 1), Some ('g', 6));
           end_tiles = (Some ('a', 8), None);
         });
    parse_test "merge2" "  h1 g6 4 a8 "
      (Move
         {
           id = 4;
           start_tiles = (Some ('h', 1), Some ('g', 6));
           end_tiles = (Some ('a', 8), None);
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
      assert_raises Malformed (fun () -> parse "  a9 33 a5 ") );
    ( "parse exception malformed move rank input 1 out of bounds" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "  i7 33 a5 ") );
    ( "parse exception malformed move rank input 2 out of bounds" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "  a7 33 a9 ") );
    ( "parse exception malformed move rank input 2 out of bounds" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "  a7 33 i5 ") );
  ]
