open State
module IntMap = Map.Make (Int)

type t = {
  pieces : quantum_piece IntMap.t;
  board : int list array array; (*access with board.(rank).(file)*)
  turn : color;
  white_kingside_castle : bool;
  white_queenside_castle : bool;
  black_kingside_castle : bool;
  black_queenside_castle : bool;
  brick_on_pipi_squares : int list;
}

(* ============================================== *)
(* ========== Private Helper Functions ========== *)
(* ============================================== *)
let int_of_file c = int_of_char c - int_of_char 'a'
let file_of_int n = n + int_of_char 'a' |> char_of_int
let pieces_at board file rank = board.board.(rank).(int_of_file file)
let qpiece_of_id board id = IntMap.find id board.pieces

let place_piece board piece_type id file rank =
  let register_piece board piece_type id file rank =
    if not (IntMap.mem id board.pieces) then
      let new_piece =
        {
          id;
          piece_type;
          superpositions = [ { file; rank; probability = 100.0 } ];
          has_moved = false;
        }
      in
      { board with pieces = IntMap.add id new_piece board.pieces }
    else
      raise
        (Failure
           ("Piece with id " ^ string_of_int id
          ^ " already exists -- cannot register"))
  in
  (*regardless, we place the piece on board (TODO, REFACTOR)*)
  board.board.(rank).(int_of_file file) <-
    id :: board.board.(rank).(int_of_file file);

  register_piece board piece_type id file rank

let update_capture_state board id capture_state =
  let piece = qpiece_of_id board id in
  let new_piece = { piece with has_moved = true } in
  { board with pieces = IntMap.add id new_piece board.pieces }

(* ========== Start of QFen module ========== *)
module QFen = struct
  exception MalformedQFen

  (* Helper functions for QFen*)
  let piece_type_of_str c =
    match c with
    | "p" -> { name = Pawn; color = Black }
    | "r" -> { name = Rook; color = Black }
    | "n" -> { name = Knight; color = Black }
    | "b" -> { name = Bishop; color = Black }
    | "q" -> { name = Queen; color = Black }
    | "k" -> { name = King; color = Black }
    | "P" -> { name = Pawn; color = White }
    | "R" -> { name = Rook; color = White }
    | "N" -> { name = Knight; color = White }
    | "B" -> { name = Bishop; color = White }
    | "Q" -> { name = Queen; color = White }
    | "K" -> { name = King; color = White }
    | _ -> raise MalformedQFen

  let str_of_piece_type pt =
    match pt with
    | { name = Pawn; color = Black } -> "p"
    | { name = Rook; color = Black } -> "r"
    | { name = Knight; color = Black } -> "n"
    | { name = Bishop; color = Black } -> "b"
    | { name = Queen; color = Black } -> "q"
    | { name = King; color = Black } -> "k"
    | { name = Pawn; color = White } -> "P"
    | { name = Rook; color = White } -> "R"
    | { name = Knight; color = White } -> "N"
    | { name = Bishop; color = White } -> "B"
    | { name = Queen; color = White } -> "Q"
    | { name = King; color = White } -> "K"

  (** [string_of_square b f r] tunrs file [f] and rank [r] of board [b] into a
      QFen tile representation*)
  let string_of_square board file rank =
    let square = pieces_at board file rank in
    let rec generate lst =
      match lst with
      | [] -> ""
      | h :: t ->
          let qpiece = qpiece_of_id board h in
          str_of_piece_type qpiece.piece_type ^ string_of_int h ^ generate t
    in
    generate square

  (** [square_of_string b f r s] parses [s], containing a QFen tile
      representation into [b] at file [f] and rank [r]*)
  let square_of_string board file rank str =
    let piece_names = Str.split (Str.regexp "[0-9]+") str in
    let piece_ids =
      let s_lst = Str.split (Str.regexp "[a-zA-Z]+") str in
      List.map (fun x -> int_of_string x) s_lst
    in
    (* print_endline ("tile: " ^ String.make 1 file ^ string_of_int rank); *)
    let rec add_piece_ids piece_lst id_lst b =
      match (piece_lst, id_lst) with
      | [], [] -> b
      | pt :: pt_lst, id :: id_lst ->
          let new_board = place_piece b (piece_type_of_str pt) id file rank in
          (* print_endline ("place piece: " ^ pt ^ (string_of_int id)); *)
          add_piece_ids pt_lst id_lst new_board
      | _ -> raise MalformedQFen
    in
    add_piece_ids piece_names piece_ids board

  let board_of_piece_str board str =
    let rows = String.split_on_char '/' str in
    let rec build_row b lst file rank =
      match lst with
      | [] -> b
      | h :: t ->
          if Str.string_match (Str.regexp "[0-9]+") h 0 then
            build_row b t (file + int_of_string h) rank
          else
            let new_board = square_of_string b (file_of_int file) rank h in
            build_row new_board t (file + 1) rank
    in

    let new_board = ref board in
    let _ =
      for x = 0 to 7 do
        (* Printf.printf "Parsing row %d: %s%!\n" x (List.nth rows x); *)
        new_board :=
          build_row !new_board
            (String.split_on_char ':' (List.nth rows x))
            0 (7 - x)
      done
    in
    !new_board

  let piece_str_of_board board =
    let rank_string = ref "" in
    let final_string = ref "" in
    let counter = ref 0 in
    for rank = 7 downto 0 do
      rank_string := "";
      counter := 0;
      for file = 0 to 7 do
        let before = !rank_string in
        let new_str = ref (string_of_square board (file_of_int file) rank) in
        (*we increment counter if this string is empty*)
        (if String.length !new_str = 0 then incr counter
        else if !counter > 0 then
          let _ = new_str := string_of_int !counter ^ ":" ^ !new_str in
          counter := 0);

        (*add to rank*)
        if before = "" then rank_string := !new_str
        else if String.get before (String.length before - 1) = ':' then
          rank_string := before ^ !new_str
        else rank_string := before ^ ":" ^ !new_str
      done;
      (*add rank to file piece string*)
      if !counter > 0 then rank_string := !rank_string ^ string_of_int !counter;
      if !final_string = "" then final_string := !rank_string
      else final_string := !final_string ^ "/" ^ !rank_string
    done;
    !final_string

  let capture_attempts_of_str board str =
    if str = "-" then board
    else
      let capture_attempts = String.split_on_char ':' str in
      let capture_attempts =
        List.map (fun x -> int_of_string x) capture_attempts
      in
      let rec update lst board =
        match lst with
        | [] -> board
        | h :: t ->
            let new_board = update_capture_state board h true in
            update t new_board
      in
      update capture_attempts board

  let str_of_capture_attempts board =
    let all_bindings = IntMap.bindings board.pieces in
    let rec gen_string lst =
      match lst with
      | [] -> ""
      | (k, v) :: t ->
          let current_string = if v.has_moved then string_of_int v.id else "" in
          let next_string = gen_string t in
          if current_string = "" then next_string
          else if next_string = "" then current_string
          else current_string ^ ":" ^ next_string
    in
    let ret_value = gen_string all_bindings in
    if ret_value = "" then "-" else ret_value

  let turn_of_str board str =
    let turn = if str = "w" then White else Black in
    { board with turn }

  let str_of_turn board =
    match board.turn with
    | White -> "w"
    | Black -> "b"

  let castling_rights_of_str board str =
    let black_kingside_castle = String.contains str 'k' in
    let black_queenside_castle = String.contains str 'q' in
    let white_kingside_castle = String.contains str 'K' in
    let white_queenside_castle = String.contains str 'Q' in
    {
      board with
      black_kingside_castle;
      black_queenside_castle;
      white_kingside_castle;
      white_queenside_castle;
    }

  let str_of_castling_rights board =
    let ret = "" in
    let ret = if board.white_kingside_castle then ret ^ "K" else ret in
    let ret = if board.white_queenside_castle then ret ^ "Q" else ret in
    let ret = if board.black_kingside_castle then ret ^ "k" else ret in
    let ret = if board.black_queenside_castle then ret ^ "q" else ret in
    if ret = "" then "-" else ret

  let pipi_of_str board str =
    if str = "-" then board
    else
      let pipi_squares =
        String.split_on_char ':' str |> List.map int_of_string
      in
      { board with brick_on_pipi_squares = pipi_squares }

  let str_of_pipi board =
    let s = board.brick_on_pipi_squares |> List.map string_of_int in
    let rec join lst delim =
      match lst with
      | [] -> ""
      | h :: t -> if t = [] then h else h ^ delim ^ join t delim
    in
    let thing = join s ":" in
    if thing = "" then "-" else thing

  (* public functions of QFen*)
  let start =
    "r0:n1:b2:q3:k4:b5:n6:r7/p8:p9:p10:p11:p12:p13:p14:p15/8/8/8/8/P16:P17:P18:P19:P20:P21:P22:P23/R24:N25:B26:Q27:K28:B29:N30:R31 \
     - w KQkq -"

  let board_from_fen fen =
    let fen_parts = String.split_on_char ' ' fen in
    let empty_board =
      {
        pieces = IntMap.empty;
        board = Array.make_matrix 8 8 [];
        black_kingside_castle = false;
        black_queenside_castle = false;
        white_kingside_castle = false;
        white_queenside_castle = false;
        turn = White;
        brick_on_pipi_squares = [];
      }
    in

    if List.length fen_parts <> 5 then raise MalformedQFen;
    (*parse piece string*)
    (* let rudimentary_board   *)
    (* Printf.printf "\n\nParsing piece string\n%!"; *)
    let board = board_of_piece_str empty_board (List.nth fen_parts 0) in
    (* Printf.printf "\n\nParsing capture_attempts\n%!"; *)
    let board = capture_attempts_of_str board (List.nth fen_parts 1) in
    (* Printf.printf "\n\nParsing turn\n%!"; *)
    let board = turn_of_str board (List.nth fen_parts 2) in
    (* Printf.printf "\n\nParsing castling rights\n%!"; *)
    let board = castling_rights_of_str board (List.nth fen_parts 3) in
    (* Printf.printf "\n\nParsing PIPI\n%!"; *)
    pipi_of_str board (List.nth fen_parts 4)

  let fen_from_board board =
    let p1 = piece_str_of_board board in
    let p2 = str_of_capture_attempts board in
    let p3 = str_of_turn board in
    let p4 = str_of_castling_rights board in
    let p5 = str_of_pipi board in
    p1 ^ " " ^ p2 ^ " " ^ p3 ^ " " ^ p4 ^ " " ^ p5
end

(* ================================================================== *)
(* ========== Public Functions that belong to module Board ========== *)
(* ================================================================== *)

let is_equal board1 board2 =
  QFen.fen_from_board board1 = QFen.fen_from_board board2

let init = QFen.board_from_fen QFen.start
let player_turn board = board.turn

let tile board square =
  match square with
  | file, rank ->
      List.fold_left
        (fun acc piece -> IntMap.find piece board.pieces :: acc)
        []
        board.board.(rank).(int_of_file file)

let piece board id = IntMap.find id board.pieces

let set_piece board piece piece' =
  { board with pieces = IntMap.add piece.id piece' board.pieces }

let top_piece board square =
  match tile board square with
  | h :: t -> h
  | _ -> failwith "error"

let piece_probability board square piece =
  (piece.superpositions |> List.find (fun pos -> (pos.file, pos.rank) = square))
    .probability

let tile_probability board square =
  tile board square
  |> List.fold_left
       (fun acc piece -> acc +. piece_probability board square piece)
       0.0

let add_piece_tile board square id probability =
  let piece = piece board id in
  match square with
  | file, rank ->
      board.board.(rank).(int_of_file file) <-
        piece.id :: pieces_at board file rank;
      let position = { file; rank; probability } in
      let piece' =
        { piece with superpositions = position :: piece.superpositions }
      in
      set_piece board piece piece'

let remove_piece_tile board square id =
  let piece = piece board id in
  match square with
  | file, rank ->
      board.board.(rank).(int_of_file file) <-
        pieces_at board file rank |> List.filter (fun id -> id <> piece.id);
      let positions =
        piece.superpositions
        |> List.filter (fun pos -> (pos.file, pos.rank) <> square)
      in
      let piece' = { piece with superpositions = positions } in
      set_piece board piece piece'

let delete_piece board piece =
  piece.superpositions
  |> List.fold_left
       (fun board_acc pos ->
         remove_piece_tile board_acc (pos.file, pos.rank) piece.id)
       board
