open State

exception Malformed of string
exception Empty of string

type move_phrase = {
  id : int;
  start_tiles : tile option * tile option;
  end_tiles : tile option * tile option;
}

type command =
| Move of move_phrase
| Help
| Observe
| Resign
| Quit

let parse str = raise (Failure "Unimplemented: Command.parse")
