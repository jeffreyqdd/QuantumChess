open OUnit2

let suite = "test suite for Quantum Chess" >::: List.flatten []
let _ = run_test_tt_main suite
