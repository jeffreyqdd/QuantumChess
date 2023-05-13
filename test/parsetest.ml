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

let string_of_coord s =
  match s with
  | a, b -> String.make 1 a ^ string_of_int b

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
    parse_test "resign" "     resign" Resign;
    ( "parse exception empty" >:: fun _ ->
      assert_raises Empty (fun () -> parse "     ") );
    ( "parse exception malformed quit" >:: fun _ ->
      assert_raises Malformed (fun () -> parse " draw     funky") );
    ( "parse exception malformed go" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "  move ") );
  ]
