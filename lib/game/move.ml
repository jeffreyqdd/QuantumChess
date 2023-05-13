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

(** [measure events] chooses the event that actually occurs out of a list of
    events, where keys correspond to events and values correspond to the
    probability the event occurs *)
let measure (events : ('k * float) list) : 'k =
  let rec make_buckets events next =
    match events with
    | [] -> []
    | (e, p) :: t -> (e, next, next +. p) :: make_buckets t (next +. p)
  in
  let buckets = make_buckets events 0.0 in
  let random = float_of_int (Random.int 100) in
  let filtered =
    List.filter
      (fun (e, start, finish) -> start <= random && random < finish)
      buckets
  in
  match filtered with
  | [ (e, start, finish) ] -> e
  | _ -> raise (Illegal "Event doesn't occur or occurs in multiple buckets")

(* ============================================== *)
(* ========== Private Helper Functions ========== *)
(* ============================================== *)

(** [is_legal_move name start finish] checks to see whether moving piece type
    [name] from [start] to [finish] is a legally allowed chess move. *)
let is_legal_move (name : piece_name) (start : coord) (finish : coord) : bool =
  match name with
  | Pawn | Rook | Knight | Bishop | Queen | King -> failwith "lol"

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
      let horizontal_move = x = x' in
      let vertical_move = y = y' in
      match name with
      | Knight -> Jump
      | Pawn -> if x = x' then N else if x' > x then NE else NW
      | Bishop -> diagonal_check x y x' y'
      | Rook -> (
          match (horizontal_move, vertical_move) with
          | true, false -> if y' > y then N else S
          | false, true -> if x' > x then E else W
          | _ ->
              failwith "dummy you can't have a rook move diagonal or stay still"
          )
      | Queen | King -> (
          match (horizontal_move, vertical_move) with
          | true, false -> if y' > y then N else S
          | false, true -> if x' > x then E else W
          | false, false -> diagonal_check x y x' y'
          | _ -> failwith "dummy you can't stay in place"))

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
    | N -> make_path direction (file_of_int (xf + 1), xr) goal acc @ [ curr ]
    | NE ->
        make_path direction (file_of_int (xf + 1), xr + 1) goal acc @ [ curr ]
    | E -> make_path direction (fst curr, xr + 1) goal acc @ [ curr ]
    | SE ->
        make_path direction (file_of_int (xf - 1), xr + 1) goal acc @ [ curr ]
    | S -> make_path direction (file_of_int (xf - 1), xr) goal acc @ [ curr ]
    | SW ->
        make_path direction (file_of_int (xf - 1), xr - 1) goal acc @ [ curr ]
    | W -> make_path direction (fst curr, xr - 1) goal acc @ [ curr ]
    | NW ->
        make_path direction (file_of_int (xf + 1), xr - 1) goal acc @ [ curr ]

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
  raise (Failure "Unimplemented: Move. specify_move")

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
      let pieces = Board.tile board (file, rank) in
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
    castle their king *)
let castle_attempt (phrase : Command.move_phrase) : bool = failwith "lol"

(* =========================================== *)
(* ========== Measurement Functions ========== *)
(* =========================================== *)

module IntMap = Map.Make (Int)

(** [full_probability_piece board square] is [Some] piece on [square] with 100%
    probability, or [None] if none exists *)
let full_probability_piece board square : quantum_piece option =
  match
    Board.tile board square
    |> List.filter (fun piece ->
           Board.piece_probability board square piece = 100.0)
  with
  | [] -> None
  | [ h ] -> Some h
  | _ -> failwith "There's more than one piece with full probability"

(** [measure_piece board square] is the piece measured to be on [square] *)
let measure_piece board square =
  match full_probability_piece board square with
  | Some piece -> Some piece
  | None ->
      let events =
        Board.tile board square
        |> List.map (fun piece ->
               (piece, Board.piece_probability board square piece))
      in
      let piece = try Some (measure events) with _ -> None in
      piece

(** [measurement board square] is the board after measurement occurs on
    [square]. We perform measurement as follows:

    Definition: The stability of a tile corresponds to the total percentage
    present on that tile. We say that a piece is stable if the probabilities on
    a tile add up to 100%. We say that a tile is unstable if the probabilities
    add up to below 100%. We say that a tile is super-stable if the probability
    exceeds 100%.

    1. Find the piece that actually exists on [square] out of all
    superpositions. There are two possible cases:

    - First, there is a piece that has 100% probability. Then, we immediately
      say this piece is the piece that exists on the board.
    - Second, no pieces have 100% probability. Then, we choose a random number
      to determine which piece actually exists.

    2. Delete all other probabilities

    - Delete the piece from all tiles except the tile corresponding to [square]
    - Delete all superpositions of the piece except for [square]
    - Delete all unused probability credits in the bank account

    3. For all other superposition pieces on [square]

    - Take the piece's probability p and add it to a bank account. The bank
      account holds probability credits, which is probability that does not
      appear on the board.
    - From the bank account, while there still exists probability credits for
      the piece, attempt to evenly spread all probability credits in the account
      to all remaining superpositions of the piece. An attempt has three
      possible outcomes:
    - First, the attempt succeeds if the stability of the tile does not exceed
      100%.
    - Second, the attempt succeeds if adding the probability credits causes a
      piece's probability on that tile to equal 100%. The tile is measured
      immediately.
    - Third, the attempt is blocked if the tile becomes super-stable without any
      specific piece to have a probability equal to 100%. In this case, we
      measure the tile immediately and end this iteration of the while loop. *)
let rec measurement (board : Board.t) (square : coord)
    (bank : float IntMap.t ref) : Board.t =
  match square with
  | file, rank -> (
      (* Find the piece that actually exists on [square] *)
      match measure_piece board square with
      | Some piece ->
          (* Delete all other probabilities of piece *)
          let board = Board.delete_piece board piece in

          (* Reallocate all other superpositions *)
          let board' = reallocate_tile board square bank in

          (* Add piece back to board *)
          let piece' = { piece with superpositions = [] } in
          Board.add_piece_tile board' square piece' 100.0
      | None ->
          (* Reallocate all superpositions on the tile *)
          let board' = reallocate_tile board square bank in
          board')

(** [reallocate_piece board square piece] pushes [piece] off of [square] and
    reallocates the probabilities to all remaining superpositions *)
and reallocate_piece board square bank piece =
  let get_curr_credits bank id =
    try IntMap.find id !bank with Not_found -> 0.0
  in
  let board = ref board in
  let probability = Board.piece_probability !board square piece in
  let curr_credits = get_curr_credits bank piece.id in

  (* Add probability to bank account and remove [piece] from [square] *)
  bank := IntMap.add piece.id (probability +. curr_credits) !bank;
  board := Board.remove_piece_tile !board square piece;

  (* Attempt to evenly spread out the probabilities in the bank account *)
  while IntMap.find piece.id !bank > 0.0 do
    let curr_balance = get_curr_credits bank piece.id in
    let num_positions = List.length piece.superpositions in
    let probability_chunk = curr_balance /. float_of_int num_positions in
    piece.superpositions
    |> List.iter (fun pos ->
           let square' = (pos.file, pos.rank) in
           let curr_probability =
             Board.piece_probability !board square' piece
           in
           (* If tile stability doesn't exceed 100% *)
           if Board.tile_probability !board square +. probability_chunk < 100.0
           then (
             board := Board.remove_piece_tile !board square' piece;
             board :=
               Board.add_piece_tile !board square' piece
                 (curr_probability +. probability_chunk);
             bank :=
               IntMap.add piece.id
                 (get_curr_credits bank piece.id -. probability_chunk)
                 !bank
             (* Else if one piece has probability = 100% on tile *))
           else if curr_probability +. probability_chunk = 100.0 then (
             board := Board.remove_piece_tile !board square' piece;
             board := Board.add_piece_tile !board square' piece 100.0;
             board := measurement !board square' bank;
             bank := IntMap.add piece.id 0.0 !bank
             (* Else if tile becomes super-stable *))
           else board := measurement !board square' bank)
  done;
  !board

(** [reallocate_tile board square bank] is the board where every piece on
    [square] is pushed off and its probabilities reallocated to other tiles *)
and reallocate_tile board square (bank : float IntMap.t ref) =
  Board.tile board square
  |> List.fold_left
       (fun board_acc piece -> reallocate_piece board_acc square bank piece)
       board

(* ================================================================= *)
(* ========== Public Functions that belong to module Move ========== *)
(* ================================================================= *)
let move board phrase = raise (Failure "Unimplemented: Move.move")
