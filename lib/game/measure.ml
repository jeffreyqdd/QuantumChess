open State
open Random
module IntMap = Map.Make (Int)

let _ = Random.self_init ()

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
      board := Board.delete_piece !board (Board.piece !board id);

      (* Push all other pieces off tile *)
      board := push_off_tile !board square bank;

      (* Add piece back to board *)
      board := Board.add_piece_tile !board square id 100.0;

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
    Board.piece_probability !board square (Board.piece !board id)
  in

  (* Add probability to bank account and remove [piece] from [square] *)
  bank := IntMap.add id (probability +. piece_credits bank id) !bank;
  board := Board.remove_piece_tile !board square id;

  (* Pre-emptively measure all tiles that would become super-stable if
     probability is added *)
  board := board_without_superstables !board bank id;

  (* Evenly spread out the probabilities in the bank account *)
  let pos_lst = (Board.piece !board id).superpositions in
  let num_positions = List.length pos_lst in
  let prob_to_add = piece_credits bank id /. float_of_int num_positions in
  let total_spent = prob_to_add *. float_of_int num_positions in
  board := add_position_probability !board id prob_to_add;
  bank := IntMap.add id (piece_credits bank id -. total_spent) !bank;

  (* If piece [id] only has 1 superposition at 100%, measure it *)
  board := measure_stable_pieces !board bank id;

  !board

(* while IntMap.find id !bank > 0.0 do let num_positions = List.length
   (Board.piece !board id).superpositions in let prob_to_add = piece_credits
   bank id /. float_of_int num_positions in

   (Board.piece !board id).superpositions |> List.iter (fun pos -> let square' =
   (pos.file, pos.rank) in let curr_probability = Board.piece_probability !board
   square' (Board.piece !board id) in (* If tile stability doesn't exceed 100%
   *) if Board.tile_probability !board square +. prob_to_add < 100.0 then (
   board := Board.remove_piece_tile !board square' id; board :=
   Board.add_piece_tile !board square' id (curr_probability +. prob_to_add);
   bank := IntMap.add id (piece_credits bank id -. prob_to_add) !bank (* Else if
   one piece has probability = 100% on tile *)) else if curr_probability +.
   prob_to_add = 100.0 then ( board := Board.remove_piece_tile !board square'
   id; board := Board.add_piece_tile !board square' id 100.0; board :=
   measure_tile !board square' bank; bank := IntMap.add id 0.0 !bank (* Else if
   tile becomes super-stable *)) else board := measure_tile !board square' bank)
   done; !board *)

(** [add_position_probability board id x] is the board where [x] probability is
    added to all superpositions of piece [id] *)
and add_position_probability board id x =
  let board = ref board in
  let add_to_pos pos =
    let square = (pos.file, pos.rank) in
    let curr_probability =
      Board.piece_probability !board square (Board.piece !board id)
    in
    board := Board.remove_piece_tile !board square id;
    board := Board.add_piece_tile !board square id (curr_probability +. x)
  in
  (Board.piece !board id).superpositions |> List.iter add_to_pos;
  !board

(** [board_without_superstables board bank id] is the board where no
    super-stable squares exist after attempting to divide the probability
    credits of piece [id] among all its superpositions *)
and board_without_superstables board bank id =
  let board = ref board in
  let pos_lst = ref (Board.piece !board id).superpositions in
  let num_positions = ref (List.length !pos_lst) in
  let to_add = ref (piece_credits bank id /. float_of_int !num_positions) in

  while find_superstable_positions !board !pos_lst !to_add <> [] do
    board :=
      find_superstable_positions !board !pos_lst !to_add
      |> List.fold_left
           (fun board_acc pos ->
             measure_tile board_acc (pos.file, pos.rank) bank)
           !board;
    pos_lst := (Board.piece !board id).superpositions;
    num_positions := List.length !pos_lst;
    to_add := piece_credits bank id /. float_of_int !num_positions
  done;

  !board

(** [find_superstable_positions board id x] is a list of positions which become
    super-stable (probability > 100%) after x is added to all positions in
    [pos_lst] *)
and find_superstable_positions board pos_lst x =
  List.filter
    (fun pos -> Board.tile_probability board (pos.file, pos.rank) +. x > 100.0)
    pos_lst

(** [measure_stable_pieces board bank id] is the board where if piece [id] has 1
    superposition at 100% while other tiles are on it, measure it *)
and measure_stable_pieces board bank id =
  let piece = Board.piece board id in
  match piece.superpositions with
  | [ pos ] ->
      let square = (pos.file, pos.rank) in
      if
        Board.piece_probability board square (Board.piece board id) = 100.0
        && List.length (Board.tile board square) > 1
      then measure_tile board square bank
      else board
  | _ -> board

(* ==================================================================== *)
(* ========== Public Functions that belong to module Measure ========== *)
(* ==================================================================== *)
let measurement board square =
  let bank = ref IntMap.empty in
  measure_tile board square bank
