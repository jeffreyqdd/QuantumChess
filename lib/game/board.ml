open State

type quantum_piece = {
  id : int;
  piece : piece;
  superpositions : (char * int) list;
  capture_attempt : bool;
}

type t = {
  pieces : quantum_piece array;
  board : int list array array;
}

let player_turn board = raise (Failure "Unimplemented: Board.player_turn")
let tile board file rank = raise (Failure "Unimplemented: Board.tile")

let file_to_int c =
  match c with
  | 'a' -> 0
  | 'b' -> 1
  | 'c' -> 2
  | 'd' -> 3
  | 'e' -> 4
  | 'f' -> 5
  | 'g' -> 6
  | 'h' -> 7
  | _ -> -1

let int_to_file n =
  match n with
  | 0 -> 'a'
  | 1 -> 'b'
  | 2 -> 'c'
  | 3 -> 'd'
  | 4 -> 'e'
  | 5 -> 'f'
  | 6 -> 'g'
  | 7 -> 'h'
  | _ -> 'z'

let rec parse_layout_str (str : char list) (board : t) (curr_file : char)
    (curr_rank : int) (piece_count : int) : t =
  let data =
    match str with
    | 'R' :: _ -> Some (Rook, Black)
    | 'N' :: _ -> Some (Knight, Black)
    | 'B' :: _ -> Some (Bishop, Black)
    | 'K' :: _ -> Some (King, Black)
    | 'Q' :: _ -> Some (Queen, Black)
    | 'r' :: _ -> Some (Rook, White)
    | 'n' :: _ -> Some (Knight, White)
    | 'b' :: _ -> Some (Bishop, White)
    | 'k' :: _ -> Some (King, White)
    | 'q' :: _ -> Some (Queen, White)
    | '.' :: _ -> None
    | '\\' :: _ -> None
    | [] -> None
    | _ -> raise (Failure "unexpected piece in starting layout")
  in

  match str with
  | [] -> board
  | _ :: t -> (
      let next_file =
        if curr_rank = 8 then int_to_file (file_to_int curr_file + 1)
        else curr_file
      in
      let next_rank = curr_rank + (1 mod 9) in

      match data with
      | Some a ->
          let current_piece =
            {
              id = piece_count;
              piece = { name = fst a; color = snd a };
              superpositions = [ (curr_file, curr_rank) ];
              capture_attempt = false;
            }
          in
          board.pieces.(current_piece.id) <- current_piece;
          board.board.(curr_rank).(file_to_int curr_file) <-
            current_piece.id :: board.board.(curr_rank).(file_to_int curr_file);
          parse_layout_str t board next_file next_rank (piece_count + 1)
      | None -> parse_layout_str t board next_file next_rank 0)

let init =
  let dummy_piece =
    {
      id = 0;
      piece = { name = Pawn; color = Black };
      superpositions = [];
      capture_attempt = false;
    }
  in
  let bare =
    { pieces = Array.make 32 dummy_piece; board = Array.make_matrix 8 8 [] }
  in
  let board_layout =
    "RNBKQBNR\\PPPPPPPP\\........\\........\\........\\........\\pppppppp\\rnbkqbnr"
  in
  let explode s = List.init (String.length s) (String.get s) in
  parse_layout_str (explode board_layout) bare 'a' 0 0
