open ANSITerminal
open Unix

(**[Normal] and [Insertion] modes are similar to vim. Normal allows moving
   around and commands, insertion allows for inputs*)
type mode =
  | Normal
  | Insertion

(**concates each line of [file_in] into a string*)
let list_of_file file_in : string list =
  let rec reader channel =
    try
      let x = input_line channel in
      x :: reader channel
    with End_of_file -> []
  in
  reader (open_in file_in)

(*Coords are in the format (col, line) where top left is (1,1)*)
module Coords = struct
  (**[origin] is the top left corner of the board*)
  let origin = (1, 1)

  (**[title] is the starting coord of the Quantum Chess Title*)
  let title = (10, 3)

  (**[command_bar] is the starting coord of where the typing bar is*)
  let command_bar = (1, 22)

  (**[command_text] is where the user input starts*)
  let command_text = (12, 22)

  (**[board_start] is the top left corner of where the board should be rendered *)
  let board_start = (13, 5)

  (*magic values which specify the direction*)
  let left = (-1, 0)
  let right = (1, 0)
  let up = (0, 1)
  let down = (0, -1)
end

(*https://www.ascii-code.com/*)
module Ascii = struct
  let escape = 27
  let backspace = 127
  let newline = Char.code '\\'
  let q = Char.code 'q'
  let a = Char.code 'a'
  let p = Char.code 'p'
  let i = Char.code 'i'
  let h = Char.code 'h'
  let j = Char.code 'j'
  let k = Char.code 'k'
  let l = Char.code 'l'
end

module Render = struct
  let background = list_of_file "assets/background.txt"
  let title = list_of_file "assets/title.txt"
  let command_bar_normal = list_of_file "assets/command_bar_normal.txt"
  let command_bar_insertion = list_of_file "assets/command_bar_insertion.txt"
  let goto (col, line) = ANSITerminal.set_cursor col line

  let rec draw (col, line) fmts lst =
    goto (col, line);
    match lst with
    | [] -> ()
    | h :: t ->
        ANSITerminal.print_string fmts h;
        draw (col, line + 1) fmts t

  let clear () = ANSITerminal.erase ANSITerminal.Screen
  let background () = draw Coords.origin [] background
  let title () = draw Coords.title [] title

  let command_bar ?(text = "") mode =
    (match mode with
    | Insertion -> draw Coords.command_bar [] command_bar_insertion
    | Normal -> draw Coords.command_bar [] command_bar_normal);
    if text <> "" then draw Coords.command_text [] [ text ];
    ()
end
