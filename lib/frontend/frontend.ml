open State
open Board

let ranks = [ 0; 1; 2; 3; 4; 5; 6; 7 ]
let files = [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h' ]

(** [piece_to_string p c] is a Unicode character representing piece [p] with
    color [c] *)
let piece_to_string (p : State.piece_name) (c : State.color) : string =
  match (c, p) with
  | Black, King -> "\u{2654}"
  | Black, Queen -> "\u{2655}"
  | Black, Rook -> "\u{2656}"
  | Black, Bishop -> "\u{2657}"
  | Black, Knight -> "\u{2658}"
  | Black, Pawn -> "\u{2659}"
  | White, King -> "\u{265A}"
  | White, Queen -> "\u{265B}"
  | White, Rook -> "\u{265C}"
  | White, Bishop -> "\u{265D}"
  | White, Knight -> "\u{265E}"
  | White, Pawn -> "\u{265F}"

(* NOT NEEDED IF SMALL BOARD, prints line separating files *)
(* let print_hline () = print_char '+'; for i = 1 to 8 do print_string "---+"
   done *)

(** [tile board file rank] is the tile represented by [file] and [rank] on some
    [board] *)
let tile (board : Board.t) (file : char) (rank : int) : string =
  match Board.tile board (file, rank) with
  | [] -> "."
  | h :: t -> piece_to_string h.piece_type.name h.piece_type.color

let string_of_board ?(render_highlight = false) board tfile trank =
  let board_str = ref [] in
  for rank = 7 downto 0 do
    let str = ref (string_of_int rank ^ " ") in
    for i = Char.code 'a' to Char.code 'h' do
      let file = Char.chr i in
      let tile_str = tile board file rank in
      let modified_tstr =
        if file = tfile && rank = 8 - trank && render_highlight then
          ANSITerminal.sprintf
            [ ANSITerminal.Background ANSITerminal.Blue ]
            "%s" tile_str
        else tile_str
      in
      str := !str ^ modified_tstr ^ " "
    done;
    board_str := !str :: !board_str
  done;
  let file_str =
    "  "
    ^ String.concat " "
        (ranks
        |> List.map (fun x -> Char.chr (Char.code 'a' + x) |> String.make 1))
  in
  List.rev (file_str :: List.rev !board_str)

let probability_at_pos (piece : quantum_piece) (file : char) (rank : int) =
  let rec helper lst =
    match lst with
    | [] -> failwith "not supposed to be here"
    | h :: t ->
        if h.file = file && h.rank = rank then h.probability else helper t
  in
  helper piece.superpositions

let tile_info board file rank =
  let this_tile = Board.tile board (file, rank) in
  let rec string_builder lst =
    match lst with
    | [] -> "\n"
    | h :: t ->
        (*we want the format: id: piece: probability*)
        let s =
          Printf.sprintf "(%2d)(%3s)(%5.2f)" h.id
            (piece_to_string h.piece_type.name h.piece_type.color)
            (probability_at_pos h file rank)
        in
        Printf.sprintf "%20s\n%s" s (string_builder t)
  in
  let ret = ref (String.split_on_char '\n' (string_builder this_tile)) in
  ret := List.rev !ret;

  for x = 0 to 10 - List.length !ret do
    ret := "                " :: !ret
  done;
  List.rev !ret

(** [print_line board rank] prints the line in [board] represented by [rank] *)
let print_line (board : Board.t) (rank : int) : unit =
  files |> List.iter (fun file -> print_string (tile board file rank));
  print_string "\n"

let draw board file rank =
  let _ = Sys.command "clear" in
  ranks |> List.iter (fun rank -> print_line board rank)
