open! Core
open! Curses

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

let ( ^ ) s1 s2 = Cons (s1, s2)

let init () =
  let colours = Curses.Color.[black; red; green; yellow; blue; magenta; cyan; white] in
  List.mapi colours ~f:(fun index colour ->
    Curses.init_pair (index + 1) colour (-1))
  |> List.reduce_exn ~f:( && )

let raw_curses win (colour_code : int) text =
  let pair_num = Curses.A.color_pair colour_code in
  Curses.wattron win pair_num;
  let status = Curses.waddstr win text in
  Curses.wattroff win pair_num;
  status

let rec waddcstr win cstr =
  match cstr with
    | Cons (cs1, cs2) -> (waddcstr win cs1) && (waddcstr win cs2)
    | Black s -> raw_curses win 1 s
    | Red s -> raw_curses win 2 s
    | Green s -> raw_curses win 3 s
    | Yellow s -> raw_curses win 4 s
    | Blue s -> raw_curses win 5 s
    | Magenta s -> raw_curses win 6 s
    | Cyan s -> raw_curses win 7 s
    | White s -> raw_curses win 8 s
