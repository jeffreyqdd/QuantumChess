(**[Normal] and [Insertion] modes are similar to vim. Normal allows moving
   around and commands, insertion allows for inputs*)
type mode =
  | Normal
  | Insertion

(**Location of all the hard coded locations for where elements are rendered on
   the screen. Contains methods to deal with coordinate manipulation as well *)
module Coords : sig
  val ( + ) : int * int -> int * int -> int * int
  (**[+ a b] is the element-wise summation of coordinates*)

  val ( = ) : int * int -> int * int -> bool
  (**[= a b] evaluates to true if the element-wise = is true for both dimensions*)

  val ( <> ) : int * int -> int * int -> bool
  (**[<> a b] evaluates to true if one of the elements does not equal the other*)

  val origin : int * int
  (**[origin] is the top left corner of the board*)

  val title : int * int
  (**[title] is the starting coord of the Quantum Chess Title*)

  val command_bar : int * int
  (**[command_bar] is the starting coord of where the typing bar is*)

  val command_text : int * int
  (**[command_text] is where the user input starts*)

  val board_start : int * int
  (**[board_start] is the top left corner of where the board should be rendered *)

  val data_start : int * int
  (**[data_start] is the top left corner of where the highlighted tile should be
     rendered *)

  (*magic values which specify the direction*)
  val left : int * int
  val right : int * int
  val up : int * int
  val down : int * int
end

(**Contains all the ascii codes needed for frontend action*)
module Ascii : sig
  val newline : int
  val escape : int
  val backspace : int
  val q : int
  val a : int
  val p : int
  val i : int
  val h : int
  val j : int
  val k : int
  val l : int
end

(**Render tools*)
module Render : sig
  val background : string list
  (**[background] is a graphic such that the first item in the list begins at
     the top of the screen.*)

  val title : string list
  (**[title] is a graphic such that the first item in the list begins at the top
     of the screen.*)

  val command_bar_normal : string list
  (**[command_bar_normal] is a graphic such that the first item in the list
     begins at the top of the screen.*)

  val command_bar_insertion : string list
  (**[command_bar_insertion] is a graphic such that the first item in the list
     begins at the top of the screen.*)

  val goto : int * int -> unit
  (**[goto (c,l)] moves the on screen coursor to col [c] and line [l]*)

  val draw : int * int -> ANSITerminal.style list -> string list -> unit
  (**[draw (c,l) fmts lst] draws [lst], with each new element in [lst] being at
     a newline. It starts at col [c] and line [l], and draws with style [fmts]*)

  val clear : unit -> unit
  (**[clear ()] clears the entire string*)

  val background : unit -> unit
  (**[background ()] draws the background*)

  val title : unit -> unit
  (**[title ()] draws the title*)

  val command_bar : ?text:string -> mode -> unit
  (**[command_bar ?text:string m] draws the command bar with text [text]. If
     [mode] is Insertion, the function uses [command_bar_insertion] graphic.
     Else, it uses [command_bar_normal] for its graphic.*)
end
