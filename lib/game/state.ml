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

type piece_type = {
  name : piece_name;
  color : color;
}
(** The name and color of a piece *)

type piece = {
  id : int;
  piece_type : piece_type;
  superpositions : (char * int) list;
  capture_attempt : bool;
}
(** The type representing a piece on the board *)

type tile = piece list
(** The type representing the pieces on a tile *)
