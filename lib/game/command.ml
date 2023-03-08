open State

exception Malformed of string
exception Empty of string

type command = {
  start_tile : tile;
  end_tile : tile;
  piece_id : int;
}

let parse str = raise (Failure "Unimplemented: Command.parse")
