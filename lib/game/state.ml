type color =
  | Black
  | White  (** The type representing the possible colors in chess *)

type piece_name =
  | Pawn
  | Rook
  | Knight
  | Bishop
  | Queen
  | King  (** The type representing the piece name on the board *)

type position = {
  file : char;
  rank : int;
  probability : float;
}
(** The probability that a piece is on the tile represented by [file] and [rank] *)

type piece = {
  id : int;
  name : piece_name;
  color : color;
  superpositions : position list;
  capture_attempt : bool;
}
(** The type representing a piece on the board *)

type tile = piece list
(** The type representing the pieces on a tile *)
