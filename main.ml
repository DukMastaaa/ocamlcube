open! Core
open! Curses

let curses_init () =
  if (Curses.use_default_colors ()) && (Curses.start_color ())
    then Coloured_text.init ();
  stdscr

let setup () =
  let stdscr = Curses.initscr () in
  if not (Curses.has_colors () && Curses.can_change_color ())
    then Result.Error "terminal cannot display or change colour"
  else
    if not ((Curses.use_default_colors ()) && (Curses.start_color ()) && Coloured_text.init ())
      then Result.Error "initialising colour failed"
    else
      Result.Ok stdscr

let main stdscr =
  stdscr |> ignore;
  let win = Window.create ~height:8 ~width:25 ~pos:{Position2.y=3; x=15} in

  Window.draw_title win "beans";
  let cstr = Coloured_text.(
    (White "white ") ^ (Red "red ") ^ (Green "green ") ^ (Yellow "yellow ")
    ^ (Blue "blue ") ^ (Magenta "magenta ") ^ (Cyan "cyan ")
  ) in
  Window.waddcstr win cstr;

  Window.wrefresh win;
  Window.wgetch win |> ignore;
  Result.Ok ()

let () =
  let open Result in
  let result = setup () >>= main in
  Curses.endwin ();
  match result with
    | Error error_str -> Printf.eprintf "Error: %s" error_str
    | Ok _ -> ()
