open State

exception Illegal of string

val move : Board.t -> Command.move_phrase -> Board.t
(** [move board phrase] is the board after a piece is moved according to
    [phrase] *)

val measurement : Board.t -> coord -> float Map.Make(Int).t ref -> Board.t
