open State

exception Illegal of string

type move_type =
  | Standard
  | Merge
  | Split  (** A move can either be a standard, merge, or split move *)

type occupancy =
  | Stable
  | Unstable
  | Empty
      (** A tile can either be stable, unstable, or empty. Stable tiles contain
          100% in probability of the existence of piece(s), Unstable tiles are
          not 100%, and Empty tiles have no pieces and are thus 0%.*)

(* ============================================== *)
(* ========== Private Helper Functions ========== *)
(* ============================================== *)

(** [is_legal_move name start finish] checks to see whether moving piece type
    [name] from [start] to [finish] is a legally allowed chess move. *)
let is_legal_move (name : piece_name) (start : coord) (finish : coord) : bool =
  match name with
  | Pawn | Rook | Knight | Bishop | Queen | King -> failwith "lol"

(** [move_path start finish] is a list of all coordinates between [start] and
    [finish] *)
let move_path (start : coord) (finish : coord) : coord list =
  raise (Failure "Unimplemented: Move.path")

(** [is_valid_move phrase] is whether the move specified by [move_phrase] is
    valid or not *)
let is_valid_move (phrase : Command.move_phrase) : bool =
  raise (Failure "Unimplmented: Move.is_valid_move")

(** [specify_move phrase] determines whether the move specified by [phrase] is a
    standard move, a merge move, or a split move *)
let specify_move (phrase : Command.move_phrase) : move_type = failwith "Unim"

(** [coord_checker square] checks whether a coordinate location on a certain
    coord has a piece, has superposition piece(s), or is empty. *)
let coord_checker (square : coord) : occupancy =
  raise (Failure "Unimplmented: Move.coord_checker")

(** [measurement board square] is the board after measurement occurs on
    [square]. Measurement checks to see whether the piece on [square] actually
    exists or not, and afterwards, it updates the board accordingly. *)
let measurement (board : Board.t) (square : coord) : Board.t =
  raise (Failure "Unimplmented: Move.measurement")

(** [capture_attempt phrase] is whether the player's move phrase is an attempt
    to capture an enemy piece *)
let capture_attempt (phrase : Command.move_phrase) : bool = failwith "lol"

(** [castle_attempt phrase] is whether the player's move phrase is an attempt to
    castle their king *)
let castle_attempt (phrase : Command.move_phrase) : bool = failwith "lol"

(* ================================================================= *)
(* ========== Public Functions that belong to module Move ========== *)
(* ================================================================= *)
let move board phrase = raise (Failure "Unimplemented: Move.move")
