open State

exception Malformed of string
exception Empty of string

type move_phrase = {
  id : int;
  start_tiles : tile option * tile option;
  end_tiles : tile option * tile option;
}
(** The phrase representing a move, split, or merge *)

type command =
  | Move of move_phrase
  | Help
  | Observe
  | Resign
  | Quit  (** The type representing a command issued to the board *)

val parse : string -> command
(** [parse s] parses [s] into a command type. The input syntax consists of
    "<start_tile> <index> <end_tile>". Each element must be deliminated by at
    least 1 space.

    - <start_tile> and <end_tile> must be 2 characters. The first character
      should be a lowercase letter in \[a, h\]. The second character should be a
      number from \[1, 8\]

    - <index> must be an index to a piece in the piece list located at the tile
      <start_tile>

    For example:

    - [e2 0 e4]

    Requires: [s] contains only alphanumeric and space characters.

    Raises: [Empty] if [s] is the empty string or contains only spaces.

    Raises: [Malformed] if the command is malformed. A command is malformed if
    it does not follow the above syntax. *)
