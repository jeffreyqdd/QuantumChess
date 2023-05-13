open State
open Random
module IntMap = Map.Make (Int)

(* ============================================== *)
(* ========== Private Helper Functions ========== *)
(* ============================================== *)

(** [piece_credits bank id] is the current balance of piece [id] in [bank] *)
let piece_credits bank id = try IntMap.find id !bank with Not_found -> 0.0

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
  | _ -> failwith "Event doesn't occur or occurs in multiple buckets"

(** [full_probability_piece board square] is [Some] piece id on [square] with
    100% probability, or [None] if none exists *)
let full_probability_piece board square =
  match
    Board.tile board square
    |> List.filter (fun piece ->
           Board.piece_probability board square piece = 100.0)
  with
  | [] -> None
  | [ h ] -> Some h.id
  | _ -> failwith "There's more than one piece with full probability"

(** [measure_piece board square] is the piece measured to be on [square] *)
let measure_piece board square =
  match full_probability_piece board square with
  | Some piece -> Some piece
  | None ->
      let events =
        Board.tile board square
        |> List.map (fun piece ->
               (piece.id, Board.piece_probability board square piece))
      in
      let id = try Some (measure events) with _ -> None in
      id

(** [measure_tile board square bank] performs measurement on [square] *)
let rec measure_tile board square bank =
  let board = ref board in
  (* Find the piece that actually exists on [square] *)
  match measure_piece !board square with
  | Some id ->
      (* Delete piece from board *)
      board := Board.delete_piece !board (Board.piece_by_id !board id);

      (* Push all other pieces off tile *)
      board := push_off_tile !board square bank;

      (* Add piece back to board *)
      let piece' = { (Board.piece_by_id !board id) with superpositions = [] } in
      board := Board.add_piece_tile !board square piece' 100.0;

      (* Result *)
      !board
  | None ->
      (* Push all other pieces off tile *)
      board := push_off_tile !board square bank;

      (* Result *)
      !board

(** [push_off_tile board square bank] is the board where every piece on [square]
    is pushed off and its probabilities reallocated to other tiles *)
and push_off_tile board square bank =
  Board.tile board square
  |> List.fold_left
       (fun board_acc piece -> push_off_piece board_acc square bank piece.id)
       board

(** [push_off_piece board square piece] pushes [piece] off of [square] and
    reallocates the probabilities to all remaining superpositions *)
and push_off_piece board square bank id =
  let board = ref board in
  let probability =
    Board.piece_probability !board square (Board.piece_by_id !board id)
  in

  (* Add probability to bank account and remove [piece] from [square] *)
  bank := IntMap.add id (probability +. piece_credits bank id) !bank;
  board := Board.remove_piece_tile !board square (Board.piece_by_id !board id);

  (* Attempt to evenly spread out the probabilities in the bank account *)
  while IntMap.find id !bank > 0.0 do
    let num_positions =
      List.length (Board.piece_by_id !board id).superpositions
    in
    let probability_chunk =
      piece_credits bank id /. float_of_int num_positions
    in
    (Board.piece_by_id !board id).superpositions
    |> List.iter (fun pos ->
           let square' = (pos.file, pos.rank) in
           let curr_probability =
             Board.piece_probability !board square'
               (Board.piece_by_id !board id)
           in
           (* If tile stability doesn't exceed 100% *)
           if Board.tile_probability !board square +. probability_chunk < 100.0
           then (
             board :=
               Board.remove_piece_tile !board square'
                 (Board.piece_by_id !board id);
             board :=
               Board.add_piece_tile !board square'
                 (Board.piece_by_id !board id)
                 (curr_probability +. probability_chunk);
             bank :=
               IntMap.add id (piece_credits bank id -. probability_chunk) !bank
             (* Else if one piece has probability = 100% on tile *))
           else if curr_probability +. probability_chunk = 100.0 then (
             board :=
               Board.remove_piece_tile !board square'
                 (Board.piece_by_id !board id);
             board :=
               Board.add_piece_tile !board square'
                 (Board.piece_by_id !board id)
                 100.0;
             board := measure_tile !board square' bank;
             bank := IntMap.add id 0.0 !bank
             (* Else if tile becomes super-stable *))
           else board := measure_tile !board square' bank)
  done;
  !board

(* ==================================================================== *)
(* ========== Public Functions that belong to module Measure ========== *)
(* ==================================================================== *)
let measurement board square =
  let bank = ref IntMap.empty in
  measure_tile board square bank
