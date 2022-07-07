open! Core
open! Curses

let acs_codes = Curses.get_acs_codes ()

type position = { y : int; x : int } [@@deriving compare]

module BoxedWindow = struct
  type t = {
    win : window;
    subwin : window;
    height : int;
    width : int;
    pos : position;  (* top-left corner in stdscr *)
    }
  
  let draw_box t =
    Curses.box t.win acs_codes.vline acs_codes.hline
  
  let create ~height ~width ~pos =
    let win = Curses.newwin height width pos.y pos.x in
    let subwin = Curses.subwin win (height - 2) (width - 2) (pos.y + 1) (pos.x + 1) in
    let t = {win; subwin; height; width; pos} in
    draw_box t;
    t
  
  let draw_title t title =
    let len = String.length title in
    let starting_col = if len < t.width then (t.width - len) / 2 else 0 in
    Curses.mvwaddstr t.win 0 starting_col title |> ignore
  
  let wnoutrefresh t = Curses.wnoutrefresh t.win |> ignore
  let wrefresh t = Curses.wrefresh t.win |> ignore
  let mvwaddstr t pos text = Curses.mvwaddstr t.subwin pos.y pos.x text |> ignore
  let waddstr t text = Curses.waddstr t.subwin text |> ignore
  let waddcstr t (cstr : Coloured_text.t) = Coloured_text.waddcstr t.subwin cstr
  let wgetch t = Curses.wgetch t.subwin
end

let curses_init () =
  let stdscr = Curses.initscr () in
  if (Curses.use_default_colors ()) && (Curses.start_color ())
    then Coloured_text.init ();
  stdscr

let () =
  let _ = curses_init () in
  let win = BoxedWindow.create ~height:8 ~width:25 ~pos:{y=3; x=15} in

  BoxedWindow.draw_title win "beans";
  let cstr = Coloured_text.(
    (White "white ") ^ (Red "red ") ^ (Green "green ") ^ (Yellow "yellow ")
    ^ (Blue "blue ") ^ (Magenta "magenta ") ^ (Cyan "cyan ")
  ) in
  BoxedWindow.waddcstr win cstr;

  BoxedWindow.wrefresh win;
  BoxedWindow.wgetch win |> ignore;
  Curses.endwin ();
