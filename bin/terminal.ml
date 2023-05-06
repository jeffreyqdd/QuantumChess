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

module Coords = struct
  (*In the format (col, line) where top left is (1,1)*)
  let origin = (1, 1)
  let title = (10, 3)
  let command_bar = (1, 22)
end

(*https://www.ascii-code.com/*)
module Ascii = struct
  let escape = 27
  let backspace = 127
  let newline = 10
  let q = Char.code 'q'
  let a = Char.code 'a'
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

  let rec draw (col, line) fmts lst =
    ANSITerminal.set_cursor col line;
    match lst with
    | [] -> ()
    | h :: t ->
        ANSITerminal.print_string fmts h;
        draw (col, line + 1) fmts t

  let clear () = ANSITerminal.erase ANSITerminal.Screen
  let background () = draw Coords.origin [] background
  let title () = draw Coords.title [] title

  let command_bar mode =
    match mode with
    | Insertion -> draw Coords.command_bar [] command_bar_insertion
    | Normal -> draw Coords.command_bar [] command_bar_normal
end
