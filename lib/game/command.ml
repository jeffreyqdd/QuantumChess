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

let explode str =
  let rec explode_inner cur_index chars =
    if cur_index < String.length str then
      let new_char = str.[cur_index] in
      explode_inner (cur_index + 1) (chars @ [ new_char ])
    else chars
  in
  explode_inner 0 []

let next =
  let counter = ref 0 in
  fun () ->
    incr counter;
    !counter

let coord_of_string s : coord =
  match explode s with
  | [] -> raise Empty
  | [ a ] -> raise Malformed
  | [ a; b ] -> (a, int_of_char b)
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
    | a :: b when a = "move" -> (
        if b = [] then raise Malformed
        else
          match b with
          | [] -> raise Malformed
          | [ c ] -> raise Malformed
          | [ c; d ] -> raise Malformed
          | c :: d :: e when d = "to" -> (
              match e with
              (* MOVE *)
              | [ f ] ->
                  Move
                    {
                      id = next ();
                      start_tiles = (Some (coord_of_string c), None);
                      end_tiles = (Some (coord_of_string f), None);
                    }
              (* MERGE *)
              | [ f; g ] ->
                  Move
                    {
                      id = next ();
                      start_tiles = (Some (coord_of_string c), None);
                      end_tiles =
                        (Some (coord_of_string f), Some (coord_of_string g));
                    }
              | _ -> raise Malformed)
          | c :: d :: e :: f when e = "to" -> (
              match f with
              (* SPLIT *)
              | [ g ] ->
                  Move
                    {
                      id = 3;
                      start_tiles =
                        (Some (coord_of_string c), Some (coord_of_string d));
                      end_tiles = (Some (coord_of_string g), None);
                    }
              | _ -> raise Malformed)
          | c :: d -> raise Malformed)
    | a :: b -> raise Malformed
  in
  f
