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

type coord = char * int
(** The type of a coordinate position on the board, represented as a tuple
    (file, rank) *)

type position = {
  file : char;
  rank : int;
  probability : float;
}
(** The probability that a piece is on the tile represented by [file] and [rank] *)

type piece_type = {
  name : piece_name;
  color : color;
}
(** The type representing a piece on the board *)

type quantum_piece = {
  id : int;
  piece_type : piece_type;
  superpositions : position list;
  capture_attempt : bool;
}
(** The type representing a quantum piece on the board *)

type tile = quantum_piece list
(** The type representing the pieces on a tile *)
