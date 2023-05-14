open OUnit2
open Boardtest
open Movetest
open Measuretest

let suite =
  "test suite for Quantum Chess"
  >::: List.flatten [ Boardtest.tests; Parsetest.tests; Measuretest.tests ]

let _ = run_test_tt_main suite
