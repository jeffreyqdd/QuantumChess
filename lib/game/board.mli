open State

type t
(** The abstract type representing the board state *)

val player_turn : t -> color
(** [player_turn board] is the player turn of the current board [t] *)

val tile : t -> char -> int -> tile
(** [tile board file rank] is the tile represented by [file] and [rank] *)

val init : t
(** [init] outputs a starting board state *)
