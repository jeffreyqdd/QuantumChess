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

(** [probability piece_locale file rank] returns the probability of a piece at a
    certain position based on [file] and [rank] in the position list
    [piece_locale]. *)
let probability (piece_locale : position list) (file : char) (rank : int) :
    float list =
  List.fold_left
    (fun acc x ->
      if x.file = file && x.rank = rank then acc @ [ x.probability ] else acc)
    [] piece_locale

(** [specify_move phrase] determines whether the move specified by [phrase] is a
    standard move, a merge move, or a split move *)
let specify_move (phrase : Command.move_phrase) : move_type =
  match (phrase.start_tiles, phrase.end_tiles) with
  | (Some x, None), (Some s, None) -> Standard
  | (Some x, Some y), (Some s, None) -> Merge
  | (Some x, None), (Some s, Some t) -> Split
  | _ -> raise (Illegal "Not a move option.")

let make_faulty_piece : quantum_piece =
  {
    id = 1;
    piece_type = { name = Pawn; color = Black };
    superpositions = [];
    has_moved = false;
  }
(* ============================================== *)
(* ========== Private Helper Functions ========== *)
(* ============================================== *)

(** [is_legal_move board piece start finish] checks to see whether moving piece
    [piece] from [start] to [finish] is a legally allowed chess move in [board]. *)
let is_legal_move (board : Board.t) (piece : quantum_piece) (start : coord)
    (finish : coord) : bool =
  let name = piece.piece_type.name in
  let color = piece.piece_type.color in
  match (start, finish) with
  | (x, y), (x', y') -> (
      let f = int_of_file x in
      let f' = int_of_file x' in
      let move_type = piece_direction name start finish in
      match name with
      | Pawn -> (
          match move_type with
          | N -> y + 1 = y' || ((not piece.has_moved) && y + 2 = y')
          | NW | NE ->
              if (f' = f + 1 && y' = y + 1) || (f' = f - 1 && y' = y + 1) then
                check_occupancy_color board finish <> color
              else false
          | _ -> false)
      | Rook -> (
          match move_type with
          | N | S | W | E -> true
          | _ -> false)
      | Knight ->
          (Int.abs f - f' = 1 && Int.abs y - y' = 2)
          || (Int.abs f - f' = 2 && Int.abs y - y' = 1)
      | Bishop -> (
          match move_type with
          | NE | NW | SE | SW -> true
          | _ -> false)
      | Queen -> (
          match move_type with
          | Jump -> false
          | _ -> true)
      | King -> (
          match move_type with
          | Jump -> false
          | E ->
              let (faulty_piece : quantum_piece) = make_faulty_piece in
              let target_rook =
                try Board.top_piece board (file_of_int (f + 3), y)
                with _ -> faulty_piece
              in
              f' = f + 1
              || (not target_rook.has_moved)
                 && target_rook.piece_type.name = Rook
                 && (not piece.has_moved)
                 && f' = f + 2
          | W ->
              let (faulty_piece : quantum_piece) =
                {
                  id = 1;
                  piece_type = { name = Pawn; color = Black };
                  superpositions = [];
                  has_moved = false;
                }
              in
              let target_rook =
                try Board.top_piece board (file_of_int (f - 4), y)
                with _ -> faulty_piece
              in
              f' = f - 1
              || (not target_rook.has_moved)
                 && target_rook.piece_type.name = Rook
                 && (not piece.has_moved)
                 && f' = f - 2
          | N | S -> Int.abs y - y' = 1
          | _ -> Int.abs y - y' = 1 && Int.abs f - f' = 1))

(** [make_path direction curr goal acc] helps to append all coord traversals of
    direction [direction] from a relative square [curr] to an end sqaure [goal]. *)
let rec make_path (direction : direction) (curr : coord) (goal : coord)
    (acc : coord list) : coord list =
  (* this case is used for when we want to involve the final tile*)
  (* if curr = goal then acc @ [ curr ] *)
  if curr = goal then acc
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

(** [move_path name start finish] is a list of all coordinates between [start]
    and [finish] in the order of traversal sequence of the piece variant [name]. *)
let move_path (name : piece_name) (start : coord) (finish : coord) : coord list
    =
  let move_direction = piece_direction name start finish in
  let include_curr = make_path move_direction start finish [] in
  match include_curr with
  | h :: t -> t
  | [] -> failwith "this is an invalid move that shouldn't have been processed"

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

(** [is_valid_traversal board piece path] returns whether or not a piece [piece]
    can move along the path [path] without any collision in state [board]. *)
let is_valid_traversal (board : Board.t) (piece : quantum_piece)
    (path : coord list) : bool =
  List.fold_left
    (fun acc x ->
      if Board.tile_probability board x < 100.0 then true else false)
    true path

(** [is_valid_finish board square piece] is whether the final location [square]
    is a valid move choice for piece [piece] on state [board]. *)
let is_valid_finish (board : Board.t) (phrase : Command.move_phrase) : bool =
  let piece = Board.piece board phrase.id in
  match (phrase.start_tiles, phrase.end_tiles) with
  | (Some x, None), (Some s, None) ->
      let occupancy = Board.tile_probability board s in
      if occupancy = 0.0 then true
      else if
        (Board.top_piece board s).piece_type.color <> piece.piece_type.color
      then true
      else if occupancy +. Board.piece_probability board x piece <= 100.0 then
        true
      else false
  | (Some x, Some y), (Some s, None) ->
      let occupancy = Board.tile_probability board s in
      if occupancy = 0.0 then true
      else if
        (Board.top_piece board s).piece_type.color <> piece.piece_type.color
      then false
      else if
        occupancy
        +. Board.piece_probability board x piece
        +. Board.piece_probability board y piece
        <= 100.0
      then true
      else false
  | (Some x, None), (Some s, Some t) ->
      let o1 = Board.tile_probability board s in
      let o2 = Board.tile_probability board t in
      if o1 = 0.0 && o2 = 0.0 then true
      else if
        (Board.top_piece board s).piece_type.color <> piece.piece_type.color
        || (Board.top_piece board t).piece_type.color <> piece.piece_type.color
      then false
      else if
        o1 +. (Board.piece_probability board x piece /. 2.0) <= 100.0
        && o2 +. (Board.piece_probability board x piece /. 2.0) <= 100.0
      then true
      else false
  | _ -> raise (Illegal "Not a move option.")

(** [is_valid_move board phrase] is whether the move specified by [move_phrase]
    is valid or not in state [board]. *)
let is_valid_move (board : Board.t) (phrase : Command.move_phrase) : bool =
  let piece = Board.piece board phrase.id in
  match (phrase.start_tiles, phrase.end_tiles) with
  | (Some x, None), (Some s, None) ->
      is_valid_traversal board piece (move_path piece.piece_type.name x s)
      && is_valid_finish board phrase
  | (Some x, Some y), (Some s, None) ->
      is_valid_traversal board piece (move_path piece.piece_type.name x s)
      && is_valid_traversal board piece (move_path piece.piece_type.name y s)
      && is_valid_finish board phrase
  | (Some x, None), (Some s, Some t) ->
      is_valid_traversal board piece (move_path piece.piece_type.name x s)
      && is_valid_traversal board piece (move_path piece.piece_type.name x t)
      && is_valid_finish board phrase
  | _ -> raise (Illegal "Not a move option.")

(** [capture_attempt phrase] is whether the player's move phrase is an attempt
    to capture an enemy piece *)
let capture_attempt (phrase : Command.move_phrase) : bool =
  failwith "no use case"

(** [castle_attempt phrase] is whether the player's move phrase is an attempt to
    castle their king. *)
let castle_attempt (phrase : Command.move_phrase) : bool =
  failwith "no use case"

(** [make_castle_option target_rook rank king_file king_file'] makes a
    move_phrase for the rooks in the case of a castle. *)
let make_castle_option (target_rook : quantum_piece) (rank : int)
    (king_file : int) (king_file' : int) : Command.move_phrase =
  let f' = king_file' in
  let f = king_file in
  let b = rank in
  {
    id = target_rook.id;
    start_tiles = (Some ((List.hd target_rook.superpositions).file, b), None);
    end_tiles =
      (if f' - f = -2 then
       ( Some
           ( file_of_int
               (int_of_file (List.hd target_rook.superpositions).file + 3),
             b ),
         None )
      else
        ( Some
            ( file_of_int
                (int_of_file (List.hd target_rook.superpositions).file - 2),
              b ),
          None ));
  }

(** [is_valid_castle board phrase] returns whether a move is a valid castle or
    not. *)
let is_valid_castle (board : Board.t) (phrase : Command.move_phrase) : bool =
  let piece = Board.piece board phrase.id in
  if piece.piece_type.name = King then
    match (phrase.start_tiles, phrase.end_tiles) with
    | (Some x, None), (Some s, None) -> (
        match (x, s) with
        | (a, b), (a', b') ->
            let f = int_of_file a in
            let f' = int_of_file a' in
            let (faulty_piece : quantum_piece) = make_faulty_piece in
            let target_rook =
              if f' - f = -2 then
                try Board.top_piece board (file_of_int (f - 4), b)
                with _ -> faulty_piece
              else if f' - f = 2 then
                try Board.top_piece board (file_of_int (f + 3), b)
                with _ -> faulty_piece
              else faulty_piece
            in
            if
              target_rook.piece_type.name = Rook
              && (not target_rook.has_moved)
              && not piece.has_moved
            then
              is_valid_finish board phrase
              && is_valid_finish board (make_castle_option target_rook b f f')
            else false)
    | (Some x, Some y), (Some s, None) -> false
    | (Some x, None), (Some s, Some t) -> (
        match (x, s, t) with
        | (a, b), (c, d), (e, f) ->
            let g = int_of_file a in
            let g' = int_of_file c in
            let g'' = int_of_file e in
            let (faulty_piece : quantum_piece) = make_faulty_piece in
            let tr1 =
              try Board.top_piece board (file_of_int (f - 4), b)
              with _ -> faulty_piece
            in
            let tr2 =
              try Board.top_piece board (file_of_int (f + 3), b)
              with _ -> faulty_piece
            in
            if
              tr1.piece_type.name = Rook && (not tr1.has_moved)
              && tr2.piece_type.name = Rook && (not tr2.has_moved)
              && not piece.has_moved
            then
              is_valid_finish board phrase
              && is_valid_finish board (make_castle_option tr1 b g g')
              && is_valid_finish board (make_castle_option tr1 b g g'')
            else false)
    | _ -> false
  else false

(* ================================================================= *)
(* ========== Public Functions that belong to module Move ========== *)
(* ================================================================= *)
let move (board : Board.t) (phrase : Command.move_phrase) : Board.t =
  let piece = Board.piece board phrase.id in
  match (phrase.start_tiles, phrase.end_tiles) with
  | (Some x, None), (Some s, None) ->
      if is_legal_move board piece x s then board else board
  | (Some x, Some y), (Some s, None) -> board
  | (Some x, None), (Some s, Some t) -> board
  | _ -> raise (Illegal "Not a move option.")
