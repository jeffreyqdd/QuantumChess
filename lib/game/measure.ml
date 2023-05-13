open State
open Random

exception Illegal of string

(* ============================================== *)
(* ========== Private Helper Functions ========== *)
(* ============================================== *)
module IntMap = Map.Make (Int)

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

(* ==================================================================== *)
(* ========== Public Functions that belong to module Measure ========== *)
(* ==================================================================== *)
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
  board :=
    Board.remove_piece_tile !board square (piece.id |> Board.piece_by_id !board);
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
             board :=
               Board.remove_piece_tile !board square'
                 (piece.id |> Board.piece_by_id !board);
             board :=
               Board.add_piece_tile !board square'
                 (piece.id |> Board.piece_by_id !board)
                 (curr_probability +. probability_chunk);
             bank :=
               IntMap.add piece.id
                 (get_curr_credits bank piece.id -. probability_chunk)
                 !bank
             (* Else if one piece has probability = 100% on tile *))
           else if curr_probability +. probability_chunk = 100.0 then (
             board :=
               Board.remove_piece_tile !board square'
                 (piece.id |> Board.piece_by_id !board);
             board :=
               Board.add_piece_tile !board square'
                 (piece.id |> Board.piece_by_id !board)
                 100.0;
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
