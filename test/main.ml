open OUnit2
open Boardtest

let suite =
  "test suite for Quantum Chess"
  >::: List.flatten [ Boardtest.tests; Parsetest.tests ]

let _ = run_test_tt_main suite
