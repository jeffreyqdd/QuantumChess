open State

val draw : Board.t -> char -> int -> unit
(** [draw board file rank] draws the current board state and highlights the
    cursor at the tile represented by [file] and [rank] *)

val tile_info : tile -> unit
(** [tile_info tile] prints the tile info for [tile] *)
