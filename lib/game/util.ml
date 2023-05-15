open State

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** pretty printer functions*)
let pp_piece p =
  let piece_str =
    match p.name with
    | Pawn -> "Pawn"
    | Rook -> "Rook"
    | Knight -> "Knight"
    | Bishop -> "Bishop"
    | Queen -> "Queen"
    | King -> "King"
  in
  let color_str =
    match p.color with
    | White -> "White"
    | Black -> "Black"
  in

  "\"" ^ "(" ^ piece_str ^ ", " ^ color_str ^ ")\""

let pp_color c =
  match c with
  | White -> "white"
  | Black -> "black"

(** The string representation of a piece_name*)
let string_of_piece_name name =
  match name with
  | Pawn -> "Pawn"
  | Rook -> "Rook"
  | Knight -> "Knight"
  | Bishop -> "Bishop"
  | Queen -> "Queen"
  | King -> "King"

(** The string of a position. *)
let string_of_position position =
  Char.escaped position.file
  ^ string_of_int position.rank
  ^ " "
  ^ string_of_float position.probability

(**[string_of_list] is the string representation of a list*)
let string_of_list element lst =
  let f x = element x in
  "[" ^ (List.map f lst |> String.concat "; ") ^ "]"

(** string_of_piece prints out a string representing a piece. *)
let string_of_piece piece =
  string_of_piece_name piece.piece_type.name
  ^ " "
  ^ string_of_list string_of_position piece.superpositions

(**[cmp_set_like_lists] evaluates to true if every unique element in [lst1] is
   in [lst2] and vice versa. Else, it evaluates to false.*)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [string_of_coord c] pretty-prints coordinate object [c]. *)
let string_of_coord c =
  match c with
  | a, b -> String.make 1 a ^ string_of_int b

(** [command_string c] pretty-prints command object [c]. *)
let command_string c =
  match c with
  | Command.Move s -> (
      match (s.start_tiles, s.end_tiles) with
      | (Some a, None), (Some c, None) ->
          string_of_coord a ^ " " ^ string_of_coord c
      | (Some a, Some b), (Some c, None) ->
          string_of_coord a ^ " " ^ string_of_coord b ^ " to "
          ^ string_of_coord c
      | (Some a, None), (Some c, Some d) ->
          string_of_coord a ^ " to " ^ string_of_coord c ^ " "
          ^ string_of_coord d
      | _, _ -> " ")
  | Draw -> pp_string "draw"
  | Resign -> pp_string "resign"
