open State

exception Malformed
exception Empty

type move_phrase = {
  id : int;
  start_tiles : coord option * coord option;
  end_tiles : coord option * coord option;
}
(** The phrase representing a move, split, or merge *)

type command =
  | Move of move_phrase
  | Draw
  | Resign  (** The type representing a command issued to the board *)

(** [explode str] splits a string [str] into its respective characters,
    returning a char list. *)
let explode str =
  let rec explode_inner cur_index chars =
    if cur_index < String.length str then
      let new_char = str.[cur_index] in
      explode_inner (cur_index + 1) (chars @ [ new_char ])
    else chars
  in
  explode_inner 0 []

(** [next] increments a counter to keep track of the turn number. Each valid
    move should increment the turn counter by 1. *)
let next =
  let counter = ref 0 in
  fun () ->
    incr counter;
    !counter

(** [coord_of_string s] turns a string [s] into a coordinate object. Raises
    [Empty] if [s] is empty Raises [Malformed] if [s] is not of the form "a..h"
    ^ "1..8" Returns coord, with char in 'a..h' and int in 1..8 *)
let coord_of_string s : coord =
  match explode s with
  | [] -> raise Empty
  | [ a ] -> raise Malformed
  | [ a; b ] ->
      let c = int_of_char b - int_of_char '0' in
      if a >= 'a' && a <= 'h' && c >= 0 && c <= 7 then (a, c)
      else raise Malformed
  | _ -> raise Malformed

(** [coord_of_string s] checks if a string [s] can be converted into a
    coordinate object. Raises [Empty] if [s] is empty Raises [Malformed] if [s]
    is not of the form "a..h" ^ "1..8" Returns true if with char in 'a..h' and
    int in 1..8, false otherwise *)
let is_coord_of_string s : bool =
  match explode s with
  | [] -> raise Empty
  | [ a ] -> raise Malformed
  | [ a; b ] ->
      let c = int_of_char b - int_of_char '0' in
      if a >= 'a' && a <= 'h' && c >= 1 && c <= 8 then true else false
  | _ -> raise Malformed

let parse str =
  String.split_on_char ' ' str
  |> List.filter (fun x -> x <> "")
  |> List.map String.lowercase_ascii
  |>
  let f x =
    match x with
    | [] -> raise Empty
    | a :: b when a = "draw" -> if b = [] then Draw else raise Malformed
    | a :: b when a = "resign" -> if b = [] then Resign else raise Malformed
    | a :: b when is_coord_of_string a -> (
        if b = [] then raise Malformed
        else
          match b with
          | [] -> raise Malformed
          | [ c ] -> raise Malformed
          | c :: d when int_of_string_opt c <> None -> (
              match d with
              (* MOVE *)
              | [ e ] ->
                  Move
                    {
                      id = int_of_string c;
                      start_tiles = (Some (coord_of_string a), None);
                      end_tiles = (Some (coord_of_string e), None);
                    }
              (* MERGE *)
              | [ e; f ] ->
                  Move
                    {
                      id = int_of_string c;
                      start_tiles = (Some (coord_of_string a), None);
                      end_tiles =
                        (Some (coord_of_string e), Some (coord_of_string f));
                    }
              | _ -> raise Malformed)
          | c :: d :: e when int_of_string_opt d <> None -> (
              match e with
              (* SPLIT *)
              | [ f ] ->
                  Move
                    {
                      id = int_of_string d;
                      start_tiles =
                        (Some (coord_of_string a), Some (coord_of_string c));
                      end_tiles = (Some (coord_of_string f), None);
                    }
              | _ -> raise Malformed)
          | c :: d -> raise Malformed)
    | a :: b -> raise Malformed
  in
  f
