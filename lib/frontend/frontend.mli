open State

val string_of_board :
  ?render_highlight:bool -> Board.t -> char -> int -> string list
(**[string_of_board b] is the string list representation of board b where each
   rank is a new string in the resulting list*)

val tile_info : Board.t -> char -> int -> string list
(** [tile_info tile] is the string representation information for [tile] *)
