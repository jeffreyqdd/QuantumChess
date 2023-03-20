open OUnit2
open Quantum
open Board

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

let q_fen_init_test (name : string) (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (QFen.init |> QFen.fen_from_board)
    ~printer:pp_string

let tests = [ q_fen_init_test "qfen init" QFen.start ]
