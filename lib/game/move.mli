exception Illegal of string

val move : Board.t -> Board.move_phrase -> Board.t
(** [move board phrase] is the board after a piece is moved according to
    [phrase] *)