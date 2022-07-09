open! Core
open Position2

let acs_codes = Curses.get_acs_codes ()

type t = {
  win : Curses.window;
  subwin : Curses.window;
  height : int;
  width : int;
  pos : Position2.t;  (* top-left corner in stdscr *)
  }

let draw_box t =
  Curses.box t.win acs_codes.vline acs_codes.hline

let getxy (pos : Position2.t) = (pos.y, pos.x)

let create ~height ~width ~pos =
  let win = Curses.newwin height width pos.y pos.x in
  let subwin = Curses.derwin win (height - 2) (width - 2) 1 1 in
  let t = {win; subwin; height; width; pos} in
  draw_box t;
  t

let draw_title t title =
  let len = String.length title in
  let starting_col = if len < t.width then (t.width - len) / 2 else 0 in
  Curses.mvwaddstr t.win 0 starting_col title

let wnoutrefresh t = Curses.wnoutrefresh t.win
let wrefresh t = Curses.wrefresh t.win
let mvwaddstr t pos text = Curses.mvwaddstr t.subwin pos.y pos.x text
let waddstr t text = Curses.waddstr t.subwin text
let waddcstr t (cstr : Coloured_text.t) = Coloured_text.waddcstr t.subwin cstr
let wgetch t = Curses.wgetch t.subwin
