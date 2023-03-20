open State
module IntMap = Map.Make (Int)

type quantum_piece = {
  id : int; (*TODO: remove this*)
  piece : piece;
  superpositions : (char * int) list;
  capture_attempt : bool;
}

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

let int_of_file c = int_of_char c - int_of_char 'a'
let file_of_int n = n + int_of_char 'a' |> char_of_int

(* ========== Public Functions that belong to module Board ========== *)

let player_turn board = board.turn

let tile board file rank =
  List.fold_left
    (fun acc q_piece -> (IntMap.find q_piece board.pieces).piece :: acc)
    []
    board.board.(rank).(int_of_file file)

(* ========== Private Helper Functions ========== *)
let pieces_at board file rank = board.board.(rank).(int_of_file file)
let piece_of_id board id = IntMap.find id board.pieces

(** [add_superposition board id file rank] Requires that piece exists*)
let add_superposition board piece_type id file rank =
  let curr_piece = IntMap.find id board.pieces in
  if curr_piece.piece <> piece_type then
    raise (Failure "Cannot superimpose pieces of different types");
  let new_piece =
    {
      curr_piece with
      superpositions = (file, rank) :: curr_piece.superpositions;
    }
  in
  { board with pieces = IntMap.add id new_piece board.pieces }

let update_capture_state board id capture_state =
  let piece = piece_of_id board id in
  let new_piece = { piece with capture_attempt = true } in
  { board with pieces = IntMap.add id new_piece board.pieces }

(** [register_piece board piece_type id file rank] Requires that piece does not
    exist *)
let register_piece board piece_type id file rank =
  if not (IntMap.mem id board.pieces) then
    let new_piece =
      {
        id;
        piece = piece_type;
        superpositions = [ (file, rank) ];
        capture_attempt = false;
      }
    in
    { board with pieces = IntMap.add id new_piece board.pieces }
  else raise (Failure "Piece already exists -- cannot register")

let place_piece board piece_type id file rank =
  if IntMap.mem id board.pieces then
    (*piece id exists, so we update super position*)
    add_superposition board piece_type id file rank
  else register_piece board piece_type id file rank

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
    | _ -> raise MalformedQFen

  (** [string_of_square b f r] tunrs file [f] and rank [r] of board [b] into a
      QFen tile representation*)
  let string_of_square board file rank =
    let square = pieces_at board file rank in
    let rec generate lst =
      match lst with
      | [] -> ""
      | h :: t ->
          let qpiece = piece_of_id board h in
          str_of_piece_type qpiece.piece ^ string_of_int h ^ generate t
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

    let rec add_piece_ids piece_lst id_lst b =
      match (piece_lst, id_lst) with
      | [], [] -> b
      | pt :: pt_lst, id :: id_lst ->
          let new_board =
            register_piece b (piece_type_of_str pt) id file rank
          in
          add_piece_ids pt_lst id_lst new_board
      | _ -> raise MalformedQFen
    in
    add_piece_ids piece_names piece_ids board

  let board_of_piece_str board str =
    let rec iter_through_col board col_lst file_num rank_num =
      match col_lst with
      | [] -> board
      | h :: t ->
          (* print_endline ("\tparsing piece: " ^ h); *)
          if Str.string_match (Str.regexp "[0-9]+") h 0 then
            iter_through_col board t (file_num + int_of_string h) rank_num
          else
            let new_board =
              square_of_string board (file_of_int file_num) rank_num h
            in
            iter_through_col new_board t (file_num + 1) rank_num
    in

    let rec iter_through_rank board rank_lst rank_num =
      match rank_lst with
      | [] -> board
      | h :: t ->
          let new_board = iter_through_rank board t (rank_num - 1) in
          (* print_endline ("parsing rank: " ^ h); *)
          iter_through_col new_board (String.split_on_char ':' h) 0 rank_num
    in

    iter_through_rank board (String.split_on_char '/' str) 7

  let piece_str_of_board board =
    (*https://stackoverflow.com/questions/10893521/how-to-take-product-of-two-list-in-ocaml*)
    let cartesian a b =
      List.concat (List.map (fun e -> List.map (fun e' -> (e, e')) b) a)
    in
    let all_tiles =
      cartesian [ 7; 6; 5; 4; 3; 2; 1; 0 ]
        [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h' ]
    in
    let rec iter_through lst =
      match lst with
      | [] -> ""
      | h :: t ->
          let rank_num = fst h in
          let file_char = snd h in
          let one_rank =
            List.fold_left
              (fun acc q_piece ->
                str_of_piece_type (IntMap.find q_piece board.pieces).piece
                ^ string_of_int (IntMap.find q_piece board.pieces).id
                ^ acc)
              ""
              board.board.(rank_num).(int_of_file file_char)
          in
          one_rank ^ iter_through t
    in
    iter_through all_tiles

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
          let current_string =
            if v.capture_attempt then string_of_int v.id else ""
          in
          let next_string = gen_string t in
          if next_string = "" then current_string
          else next_string ^ current_string
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
    let ret = if board.black_kingside_castle then ret ^ "k" else ret in
    let ret = if board.black_queenside_castle then ret ^ "q" else ret in
    let ret = if board.white_kingside_castle then ret ^ "K" else ret in
    let ret = if board.white_queenside_castle then ret ^ "W" else ret in
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
    "r0:n1:b2:k3:q4:b5:n6:r7/p8:p9:p10:p11:p12:p13:p14:p15:/8/8/8/8/P16:P17:P18:P19:P20:P21:P22:P23/R24:N25:B26:K27:Q28:B29:N30:R31 \
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

  let init = board_from_fen start
end
