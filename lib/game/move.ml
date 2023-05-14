open State
open Random

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

type direction =
  | N
  | NE
  | E
  | SE
  | S
  | SW
  | W
  | NW
  | Jump
      (** A piece that has vector based movements can only be moving in one of
          these directions. The only piece with an exception to this is the
          knight who classifies as a Jump.*)

(* =============================================== *)
(* ========== Private Utility Functions ========== *)
(* =============================================== *)

(** [int_of_file c] converts file [c] to an integer value *)
let int_of_file c = int_of_char c - int_of_char 'a'

(** [file_of_int n] converts an integer [n] to a file value *)
let file_of_int n = n + int_of_char 'a' |> char_of_int

(** [tile_of_coord board square] is the tile that represents the coord [square]
    in [board]. *)
let tile_of_coord (board : Board.t) (square : coord) : tile =
  match square with
  | file, rank -> Board.tile board (file, rank)

(** [check_occupancy_color board square] returns what color the pieces on the
    board are. If color accumulator ever becomes White and then matches a
    different piece to Black in the tile of coord [square] in [board], then
    throws an error. *)
let check_occupancy_color (board : Board.t) (square : coord) : color =
  let pieces = Board.tile board square in
  List.fold_left
    (fun acc x ->
      match x.piece_type.color with
      | Black ->
          if acc = Black then Black
          else failwith "There is a color conflict tile"
      | White -> White)
    Black pieces

(* ============================================== *)
(* ========== Private Helper Functions ========== *)
(* ============================================== *)

(** [is_legal_move name start finish] checks to see whether moving piece type
    [name] from [start] to [finish] is a legally allowed chess move. *)
let is_legal_move (name : piece_name) (start : coord) (finish : coord) : bool =
  match (start, finish) with
  | (x, y), (x', y') -> (
      match name with
      | Pawn -> if y + 1 = y' then true else false
      | Rook | Knight | Bishop | Queen | King -> failwith "lol")

(** [diagonal_check x y x' y'] returns the direction type for a diagonal
    movement, from a piece able to traverse in all diagonals, based on the
    provided before and after coordinates [x] [x'] [y] [y']. *)
let diagonal_check (x : char) (y : int) (x' : char) (y' : int) : direction =
  if x' > x && y' > y then NE
  else if x' > x && y' < y then SE
  else if x' < x && y' > y then NW
  else SW

(** [piece_direction name start finish] determines the directional movement of a
    piece [name] by returning an instance of the direction type based on [start]
    and [finish] coordinates. *)
let piece_direction (name : piece_name) (start : coord) (finish : coord) :
    direction =
  match (start, finish) with
  | (x, y), (x', y') -> (
      let horizontal_move = y = y' in
      let vertical_move = x = x' in
      match name with
      | Knight -> Jump
      | Pawn -> if x = x' then N else if x' > x then NE else NW
      | Bishop -> diagonal_check x y x' y'
      | Rook -> (
          match (horizontal_move, vertical_move) with
          | false, true -> if y' > y then N else S
          | true, false -> if x' > x then E else W
          | _ -> raise (Illegal "can't have a rook move diagonal or stay still")
          )
      | Queen | King -> (
          match (horizontal_move, vertical_move) with
          | false, true -> if y' > y then N else S
          | true, false -> if x' > x then E else W
          | false, false -> diagonal_check x y x' y'
          | _ -> raise (Illegal "can't stay in place")))

(** [make_path direction curr goal acc] helps to append all coord traversals of
    direction [direction] from a relative square [curr] to an end sqaure [goal]. *)
let rec make_path (direction : direction) (curr : coord) (goal : coord)
    (acc : coord list) : coord list =
  if curr = goal then acc @ [ curr ]
  else
    let xf = int_of_file (fst curr) in
    let xr = snd curr in
    match direction with
    | Jump -> make_path direction goal goal acc
    | N -> make_path direction (fst curr, xr + 1) goal acc @ [ curr ]
    | NE ->
        make_path direction (file_of_int (xf + 1), xr + 1) goal acc @ [ curr ]
    | E -> make_path direction (file_of_int (xf + 1), xr) goal acc @ [ curr ]
    | SE ->
        make_path direction (file_of_int (xf + 1), xr - 1) goal acc @ [ curr ]
    | S -> make_path direction (fst curr, xr - 1) goal acc @ [ curr ]
    | SW ->
        make_path direction (file_of_int (xf - 1), xr - 1) goal acc @ [ curr ]
    | W -> make_path direction (file_of_int (xf - 1), xr) goal acc @ [ curr ]
    | NW ->
        make_path direction (file_of_int (xf - 1), xr + 1) goal acc @ [ curr ]

(** [move_path start finish] is a list of all coordinates between [start] and
    [finish] in the order of traversal sequence. *)
let move_path (name : piece_name) (start : coord) (finish : coord) : coord list
    =
  let move_direction = piece_direction name start finish in
  let include_curr = make_path move_direction start finish [] in
  match include_curr with
  | h :: t -> t
  | [] -> failwith "this is an invalid move that shouldn't have been processed"

(** [is_valid_move phrase] is whether the move specified by [move_phrase] is
    valid or not *)
let is_valid_move (phrase : Command.move_phrase) : bool =
  raise (Failure "Unimplmented: Move.is_valid_move")

(** [specify_move phrase] determines whether the move specified by [phrase] is a
    standard move, a merge move, or a split move *)
let specify_move (phrase : Command.move_phrase) : move_type =
  match (phrase.start_tiles, phrase.end_tiles) with
  | (Some x, None), (Some s, None) -> Standard
  | (Some x, Some y), (Some s, None) -> Merge
  | (Some x, None), (Some s, Some t) -> Split
  | _ -> raise (Illegal "Not a move option.")

(** [probability piece_locale file rank] returns the probability of a piece at a
    certain position based on [file] and [rank] in the position list
    [piece_locale]. *)
let probability (piece_locale : position list) (file : char) (rank : int) :
    float list =
  List.fold_left
    (fun acc x ->
      if x.file = file && x.rank = rank then acc @ [ x.probability ] else acc)
    [] piece_locale

(** [coord_checker board square] checks whether a coordinate location on a
    certain coord [square] has a piece, has superposition piece(s), or is empty
    in the board state [board]. *)
let coord_checker (board : Board.t) (square : coord) : occupancy =
  match square with
  | file, rank -> (
      let pieces = tile_of_coord board (file, rank) in
      let max_percentage_list =
        List.fold_left
          (fun acc qpiece -> probability qpiece.superpositions file rank)
          [] pieces
      in
      match List.fold_left (fun acc x -> acc +. x) 0.0 max_percentage_list with
      | 0.0 -> Empty
      | 100.0 -> Stable
      | _ -> Unstable)

(* let rec measure_piece_old (board : Board.t) (piece : quantum_piece) : Board.t
   = let events = List.map (fun x -> ((x.file, x.rank), x.probability))
   piece.superpositions in let true_coord = measure events in (* Remove [piece]
   from all tiles in board *) let tile' board file rank = Board.tile board
   (file, rank) |> List.filter (fun x -> x.id <> piece.id) in let board =
   List.fold_left (fun acc position -> tile' acc position.file position.rank |>
   Board.set_tile acc (position.file, position.rank)) board piece.superpositions
   in (* Add [piece] back to the tile of [true_coord] *) let piece' :
   quantum_piece = match true_coord with | f, r -> { piece with superpositions =
   [ { file = f; rank = r; probability = 100.0 } ]; } in let tile'' board file
   rank = Board.tile board (file, rank) @ [ piece' ] in let board' = match
   true_coord with | f, r -> Board.set_tile board (f, r) (tile'' board f r) in
   (* Remove other pieces that are also occupying [true_coord] *) (* Make it so
   that other pieces have probabilities divided amongst all remaining
   superpositions. Recursively measure those that got kicked off *) board' *)

(** [capture_attempt phrase] is whether the player's move phrase is an attempt
    to capture an enemy piece *)
let capture_attempt (phrase : Command.move_phrase) : bool = failwith "lol"

(** [castle_attempt phrase] is whether the player's move phrase is an attempt to
    castle their king. *)
let castle_attempt (phrase : Command.move_phrase) : bool = failwith "lol"

(* ================================================================= *)
(* ========== Public Functions that belong to module Move ========== *)
(* ================================================================= *)
let move board phrase = raise (Failure "Unimplemented: Move.move")
