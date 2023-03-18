open State

type t
(** The abstract type representing the board state *)

(**[QFen] encapsulates all operations related to serializing and deserializing
   the board position. FEN stands for Forsyth-Edwards Notation, and it is used
   to represent the chess state in a string. For quantum chess, the FEN notation
   used will be modified into QuantumFen support to superpositions.

   The syntax of QFen is explained below. Firstly, we will define some formats

   - a piece is denoted by 2-3 characters. Char 1 is an element of
     \{rnbpkqRNBPKQ\}. The lowercase refers to the black pieces and the
     uppercase refer to the white pieces. The second to third char is an id. For
     example: n1 is a black knight with an id of 1.

   - Important: pieces with the same id are denote its superpositions. However,
     the piece type and color must be the same.

   A QFen consists of "<piece string> <capture_attemps> <turn> <castling rights>
   <en passant>". Each section is space deliminated

   - piece string: pieces are separated by a colon. A forward slash denotes next
     rank. A number by itself denotes the number of empty squares. There can be
     mutiple pieces per tile. The string starts on the a8 square and makes its
     way to the h1 square. eg. a board with 8 black pawns on their starting
     position: "8/p0:p0:p2:p3:p4:p5:p6:p7/8/8/8/8/8/8" note that the pawns on a7
     and b7 are the same pawns but in superposition.

   - capture_attempts: numbers corresponding to piece ids deliminated by colons.
     Each number refers to the piece that is currently. If a piece is captured
     and not in superposition, it is removed from play. If a piece is captured
     and it is in superposition, then the piece is referred to in
     capture_attempts. For example 0:31 refers to pieces with id 0 and 31 being
     captured. A dash "-" refers to empty.

   - turn: either b or w
   - castling rights: QK denote castling queenside/kingside for wide. Same for
     black, but with lowercase variances. For example: QKk means white has all
     castling rights, but black can only castle kingside. A dash "-" refers to
     empty

   - en passant: numbers corresponding to pawn ids deliminated by colons that
     are en passantable. A dash "-" to empty.*)

module QFen : sig
  val start : string
  (**[start] evalues to a QFen denoting a normal starting position*)

  val board_from_fen : string -> t
  (**[board_from_fen f] deserializes [f] to [Board.t].*)

  val fen_from_board : t -> string
  (**[fen_from_board b] serializes [b] into a QFen.*)

  val init : t
  (**[init] evaluates to the board state denoted in the fen [start]*)
end

val player_turn : t -> color
(** [player_turn board] is the player turn of the current board [t] *)

val tile : t -> char -> int -> tile
(** [tile board file rank] is the tile represented by [file] and [rank] *)
