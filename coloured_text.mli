(** Coloured text type *)
type t =
  | Black of string
  | Red of string
  | Green of string
  | Yellow of string
  | Blue of string
  | Magenta of string
  | Cyan of string
  | White of string
  | Cons of t * t

(** Alias of Cons *)
val ( ^ ) : t -> t -> t

val init : unit -> bool

(** Prints out the coloured string on the given window *)
val waddcstr : Curses.window -> t -> bool
