open OUnit2
open Boardtest
open Movetest
open Measuretest

(* TEST PLAN

   =========

   Our test plan was as follows. We decided to split our code into several
   modules in order to better facilitate coordination and cooperation between
   the different members of the team in developing Quantum Chess. These modules
   included Board, Command, Measure, and Move. Board is responsible for storing
   and representing the board state, including pieces, tiles, and player turns.
   Here, we developed test cases for all public interface functions that we were
   able to test, including Board.is_equal, Board.player_turn, and Board.tile.
   Since Board.t is an abstract type, testing some of our public interface
   functions required utilizing other helpers in Board. For example, our test
   for Board.piece_probability required us to input the actual piece on a7,
   which required us to utilize Board.top_piece. Overall, we tested Board to
   ensure that all of the public helper functions work as intended.

   For Frontend, we decided not to write any test cases because the entire
   module was visual, so there was nothing to test. However, this does not
   detract from the overall correctness of our program because all bugs in the
   Frontend module can clearly be seen when playing the game.

   For Command, we additionally wrote several test cases for Command.parse, the
   only public interface function for the module, to ensure that all correct
   commands are parsed into the correct corresponding Move phrase while all
   incorrect and malformed commands are rejected. This ensures that our parsing
   component of Quantum Chess works as expected.

   Finally, for Move, and Measure, we created a different approach to testing.
   Since Quantum Chess utilizes probabilities, it was impossible for us to
   compare the output of a move or measurement to some expected board state.
   This is because due to the way that the wavefunctions collapse, there can be
   several possible board states as certain pieces are measured to be in certain
   positions, and it is simply not possible to effectively identify every single
   valid board state in the test cases. As a result, we opted to go for a
   different approach wherein we utilized "no-fail" tests to ensure that a move
   or measurement does not lead to any errors. In order to guarantee that the
   move or measurement actually works as expected, we utilized a wide degree of
   interactive debugging in order to ensure that moves and measures are correct.
   only

   In summary, we used OUnit to test Board, Command, Measure, and Move. We
   additionally manually tested Frontend, Move, and Measure to ensure that the
   modules work as expected. We developed our test cases using black box testing
   by creating test cases based only on the public interface functions that were
   exposed in the mli file. By utilizing a combination of OUnit test suites and
   extensive manual testing, we effectively guarantee the correctness of our
   system. *)

let suite =
  "test suite for Quantum Chess"
  >::: List.flatten [ Boardtest.tests; Measuretest.tests; Movetest.tests ]

let _ = run_test_tt_main suite
