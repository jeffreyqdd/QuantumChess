open Quantum
open Board
open Frontend

let _ =
  let board = init in
  ANSITerminal.print_string [ ANSITerminal.red ] "Welcome to quantum chess!\n";
  draw board 'a' 0