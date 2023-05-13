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

let string_of_piece_name name =
  match name with
  | Pawn -> "Pawn"
  | Rook -> "Rook"
  | Knight -> "Knight"
  | Bishop -> "Bishop"
  | Queen -> "Queen"
  | King -> "King"

let string_of_position position =
  Char.escaped position.file
  ^ string_of_int position.rank
  ^ " "
  ^ string_of_float position.probability

let string_of_list element lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ element h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ element h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let string_of_piece piece =
  string_of_piece_name piece.piece_type.name
  ^ " "
  ^ string_of_list string_of_position piece.superpositions
