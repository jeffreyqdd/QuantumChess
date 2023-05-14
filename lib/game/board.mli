open State

type t
(** The abstract type representing the board state *)

(** [QFen] encapsulates all operations related to serializing and deserializing
    the board position. FEN stands for Forsyth-Edwards Notation, and it is used
    to represent the chess state in a string. For quantum chess, the FEN
    notation used will be modified into QuantumFen support to superpositions.

    The syntax of QFen is explained below. Firstly, we will define some formats

    - a piece is denoted by 2-3 characters. Char 1 is an element of
      \{rnbpkqRNBPKQ\}. The lowercase refers to the black pieces and the
      uppercase refer to the white pieces. The second to third char is an id.
      For example: n1 is a black knight with an id of 1.

    - Important: pieces with the same id are not allowed. Superpositions cannot
      be encoded as a string. Doing so will "destroy" the superposition.

    A QFen consists of "<piece string> <capture_attemps> <turn> <castling
    rights> <en passant>". Each section is space deliminated

    - piece string: pieces (tiles?) are separated by a colon. A forward slash
      denotes next rank. A number by itself denotes the number of empty squares.
      There can be mutiple pieces per tile. The string starts on the a8 square
      and makes its way to the h1 square. eg. (a board with 8 black pawns on
      their starting position and a black knight in superposition:
      "8/p0:p1:p2:p3:p4:p5:p6:p7/8/8/8/8/8/8")

    - capture_attempts: numbers corresponding to piece ids deliminated by
      colons. Each number refers to the piece that is currently. If a piece is
      captured and not in superposition, it is removed from play. If a piece is
      captured and it is in superposition, then the piece is referred to in
      capture_attempts. For example 0:31 refers to pieces with id 0 and 31 being
      captured. A dash "-" refers to empty.

    - turn: either b or w
    - castling rights: QK denote castling queenside/kingside for wide. Same for
      black, but with lowercase variances. For example: QKk means white has all
      castling rights, but black can only castle kingside. A dash "-" refers to
      empty

    - en passant: numbers corresponding to pawn ids deliminated by colons that
      are en passantable. A dash "-" to empty. *)

module QFen : sig
  exception MalformedQFen
  (** Raised when a QFen cannot be parsed due to it violating the above
      specifications *)

  val start : string
  (** [start] evaluates to a QFen denoting a normal starting position *)

  val board_from_fen : string -> t
  (** [board_from_fen f] deserializes [f] to [Board.t]. *)

  val fen_from_board : t -> string
  (** [fen_from_board b] serializes [b] into a QFen. *)
end

val is_equal : t -> t -> bool
(** [is_equal board1 board2] is true if [board1 = board2] and false otherwise. *)

val init : t
(** [init] evaluates to the board state denoted in the fen [start] *)

val player_turn : t -> color
(** [player_turn board] is the player turn of the current board [t] *)

val tile : t -> coord -> tile
(** [tile board square] is the tile at [square].*)

val piece : t -> int -> quantum_piece
(** [piece board id] is the piece of [id]. Fails if no piece is found. *)

val pieces : t -> quantum_piece list
(** [pieces board] is the list of all pieces in [board] *)

val set_piece : t -> quantum_piece -> quantum_piece -> t
(** [set_piece board piece piece'] is the board where [piece] is replaced with
    [piece']. *)

val top_piece : t -> coord -> quantum_piece
(** [top_piece board square] is the top-most piece at [square]. Fails if no
    piece is found. *)

val piece_probability : t -> coord -> quantum_piece -> float
(** [piece_probability board square piece] is the probability that [piece] is at
    [square]. *)

val tile_probability : t -> coord -> float
(** [tile_probability board square] is the total probability of all pieces at
    [square] *)

val add_piece_tile : t -> coord -> int -> float -> t
(** [add_piece_tile board square id probability] is the board where [piece] is
    added to the tile at [square], and [square] is added to
    [piece.superpositions] with [probability]. *)

val remove_piece_tile : t -> coord -> int -> t
(** [remove_piece_tile board square int] is the board where [piece] is removed
    from the tile at [square], and [square] is removed from
    [piece.superpositions]. *)

val delete_piece : t -> quantum_piece -> t
(** [delete_piece board piece] is the board where [piece] is removed from all
    tiles. *)
