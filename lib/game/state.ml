type color =
  | Black
  | White  (** The type representing the possible colors in chess *)

type piece_name =
  | None
  | Pawn
  | Rook
  | Knight
  | Bishop
  | Queen
  | King  (** The type representing the piece name on the board *)

type piece = {
  name : piece_name;
  color : color;
}
(** The type representing a piece on the board *)

type tile = piece list
(** The type representing the pieces on a tile *)