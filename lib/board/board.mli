type board
(** The abstract type representing the board state *)

type piece_name =
  | None
  | Pawn
  | Rook
  | Knight
  | Bishop
  | Queen
  | King  (** The type representing the piece name on the board *)

type color =
  | Black
  | White  (** The type representing the possible colors in chess *)

type piece = {
  name : piece_name;
  color : color;
}
(** The type representing a piece on the board *)

type tile = piece list
(** The abstract type representing a tile in the board *)

val player_turn : board -> color
(** [player_turn board] is the player turn of the current board [t] *)

val tile : board -> char -> int -> tile
(** [tile board file rank] is the tile represented by [file] and [rank] *)

val init : board
(** [init] outputs a starting board state *)
