open State

val draw : Board.t -> char -> int -> unit
(** [draw board file rank] draws the board state given the current state [board]
    where the cursor is at the tile represented by [file] and [rank] *)

val tile_info : tile -> unit
(** [tile_info tile] prints the tile info for [tile] *)