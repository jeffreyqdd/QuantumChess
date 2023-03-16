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
  board : int list array array;
}

let player_turn board = raise (Failure "Unimplemented: Board.player_turn")
let tile board file rank = raise (Failure "Unimplemented: Board.tile")
let file_to_int c = int_of_char c - int_of_char 'a'
let int_to_file n = n + int_of_char 'a' |> char_of_int

module QFen = struct
  exception MalformedQFen

  let start =
    "r0:n1:b2:k3:q4:b5:n6:r7/p8:p9:p10:p11:p12:p13:p14:p15:/8/8/8/8/P16:P17:P18:P19:P20:P21:P22:P23/R24:N25:B26:K27:Q28:B29:N30:R31 \
     - QKqk - w"

  (*helper functions for fen_from_board begin here*)
  let rec iter_through_col board col_lst rank_num file_num =
    let parse_piece str =
      let piece_letter = String.sub str 0 1 in
      let number_letters =
        String.sub str 1 (String.length str - 1) |> int_of_string
      in
      let piece_type =
        match piece_letter with
        | "p" -> { color = Black; name = Pawn }
        | "r" -> { color = Black; name = Rook }
        | "n" -> { color = Black; name = Knight }
        | "b" -> { color = Black; name = Bishop }
        | "q" -> { color = Black; name = Queen }
        | "k" -> { color = Black; name = King }
        | "P" -> { color = White; name = Pawn }
        | "R" -> { color = White; name = Rook }
        | "N" -> { color = White; name = Knight }
        | "B" -> { color = White; name = Bishop }
        | "Q" -> { color = White; name = Queen }
        | "K" -> { color = White; name = King }
        | _ -> raise MalformedQFen
      in
      (number_letters, piece_type)
    in

    match col_lst with
    | [] -> board
    | h :: t ->
        (*figure out if is piece or column skip*)
        let is_column_skip = Str.string_match (Str.regexp "[0-9]+$") h 0 in
        if is_column_skip then
          iter_through_col board t rank_num (file_num + int_of_string h)
        else
          (* if piece, parse*)
          let id, piece_type = parse_piece h in
          (* check if piece already exists, if so, then it is in superposition*)
          if IntMap.mem id board.pieces then
            let piece = IntMap.find id board.pieces in
            (*ensure piece color and type is satisfied*)
            let _ = if piece.piece <> piece_type then raise MalformedQFen in
            (*update piece with new superposition*)
            let new_piece =
              {
                piece with
                superpositions =
                  (int_to_file file_num, rank_num) :: piece.superpositions;
              }
            in
            { board with pieces = IntMap.add id new_piece board.pieces }
          else
            (*create piece that is not in superposition*)
            let new_piece =
              {
                id;
                piece = piece_type;
                superpositions = [ (int_to_file file_num, rank_num) ];
                capture_attempt = false;
              }
            in
            (*place piece on board*)
            (*todo make this into an actual function*)
            board.board.(rank_num).(file_num) <-
              id :: board.board.(rank_num).(file_num);

            { board with pieces = IntMap.add id new_piece board.pieces }

  let rec iter_through_rank board rank_lst rank_num =
    match rank_lst with
    | [] -> board
    | h :: t ->
        let new_board = iter_through_rank board t (rank_num - 1) in
        iter_through_col new_board (String.split_on_char ':' h) rank_num 0

  (**[parse_rough_setup str] parses [str] into pieces and board, see type 'a t.
     Requires [str] has a valid <piece_string> format.*)
  let parse_rough_setup str =
    let empty_board =
      { pieces = IntMap.empty; board = Array.make_matrix 8 8 [] }
    in
    let ranks = String.split_on_char '/' str in
    iter_through_rank empty_board ranks 7

  (* MARKS THE START OF PUBLIC FUNCTIONS*)
  let valid_fen fen = raise (Failure "Yeet")
  let board_from_fen fen = raise (Failure "yeet")
  let fen_from_board board = raise (Failure "yeet")
  let init = board_from_fen start
end
