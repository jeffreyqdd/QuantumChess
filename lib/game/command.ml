open State

exception Malformed of string
exception Empty of string

type move_phrase = {
  id : int;
  start_tiles : coord option * coord option;
  end_tiles : coord option * coord option;
}
(** The phrase representing a move, split, or merge *)

type command =
  | Move of move_phrase
  | Help
  | Observe
  | Resign
  | Quit  (** The type representing a command issued to the board *)

let parse str = raise (Failure "Unimplemented: Command.parse")
