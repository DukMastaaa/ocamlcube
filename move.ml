open! Core

type action = 
  | Up
  | Front
  | Right
  | Back
  | Left
  | Down
  | SliceM
  | SliceE
  | SliceS
  | RotX
  | RotY
  | RotZ

module Lexical_analysis = struct
  open Angstrom

  (** the ints are the actual ints written, 1-indexed *)
  type depth =
    | BlankDepth
    | Layer of int
    | Range of int * int
  
  type repeat =
    | BlankRepeat
    | Number of int

  type lexical_move = {
    depth : depth;
    action : action;
    wide : bool;
    repeat : repeat;
    inverse : bool;
  }

  let parse_wide_char = peek_char >>= function
    | Some 'w' -> advance 1 *> return true
    | _ -> return false

  let parse_action_and_wide : (action * bool) Angstrom.t =
    any_char >>= fun act -> parse_wide_char >>= fun w ->
      let return_w a = return (a, w) in
      let return_no_w a =
        if w then fail (Printf.sprintf "w not allowed after '%c' action" act)
        else return (a, true) in 
      match act with
        | 'U' -> return_w Up
        | 'F' -> return_w Front
        | 'R' -> return_w Right
        | 'B' -> return_w Back
        | 'L' -> return_w Left
        | 'D' -> return_w Down
        | 'M' -> return_w SliceM
        | 'E' -> return_w SliceE
        | 'S' -> return_w SliceS
        | 'x' -> return_w RotX
        | 'y' -> return_w RotY
        | 'z' -> return_w RotZ
        | 'u' -> return_no_w Up
        | 'f' -> return_no_w Front
        | 'r' -> return_no_w Right
        | 'b' -> return_no_w Back
        | 'l' -> return_no_w Left
        | 'd' -> return_no_w Down
        | _ -> fail "no valid action"
  
  let digit = take_while1 Char.is_digit >>| Int.of_string

  let parse_depth =
    peek_char
    >>= function
      | Some '-' -> fail "unexpected `-' character"
      | Some ('0'..'9') ->
        digit >>= fun start ->
        peek_char
        >>= (function
          | Some '-' -> advance 1 *> digit >>| fun stop -> Range (start, stop)
          | _ -> return (Layer start))
      | _ -> return BlankDepth

  let parse_repeat = option BlankRepeat (digit >>| fun n -> Number n)

  let parse_inverse = 
    peek_char >>= function
      | Some '\'' -> advance 1 *> return true
      | _   -> return false

  let parse_move =
    lift4 (fun depth (action, wide) repeat inverse -> {depth; action; wide; repeat; inverse})
      (parse_depth) (parse_action_and_wide) (parse_repeat) (parse_inverse)
end

(** layers are 0-indexed *)
type depth = Range of int * int

type move = {
  depth   : depth;
  action  : action;
  repeat  : int;
  inverse : bool;
}

module Semantic_analysis = struct

  (** the Result module doesn't have this ._. *)
  let ( let* ) o f = Result.(>>=) o f

  let wide_allowed = function
    | Up | Front | Right | Back | Left | Down -> true
    | _ -> false

  let depth_is_blank = function
    | Lexical_analysis.BlankDepth -> true
    | _ -> false

  let depth_is_range = function
    | Lexical_analysis.Range _ -> true
    | _ -> false

  let get_repeat_number = function
    | Lexical_analysis.BlankRepeat -> Result.Ok 0
    | Number 0 -> Result.Error "invalid repeat 0"
    | Number n -> Result.Ok n

  let get_actual_depth action wide depth =
    if not (wide_allowed action) && not (depth_is_blank depth)
      then Result.Error "action cannot have depth modifier"
    else if not (wide_allowed action) && wide
      then Result.Error "action cannot be wide"
    else if depth_is_range depth && not wide
      then Result.Error "specifying a range requires w"
    else match depth with
      | Range (start, end_) ->
        if start = 0 || end_ = 0 then Result.Error "invalid layer 0"
        else if start > end_ then Result.Error "start of range must not be more than end"
        else Result.Ok (Range (start-1, end_-1))
      | Layer layer ->
        if layer = 0 then Result.Error "invalid layer 0"
        else Result.Ok (Range (layer-1, layer-1))
      | BlankDepth ->
        Result.Ok (Range (0, 0))

  let process_lex_output Lexical_analysis.{depth; action; wide; repeat; inverse} =
    let open Result in
    let* repeat = get_repeat_number repeat in
    let* new_depth = get_actual_depth action wide depth in
    Result.Ok {depth=new_depth; action; repeat; inverse}
end

let move_of_string s =
  let open Result in
  Angstrom.parse_string ~consume:Angstrom.Consume.Prefix Lexical_analysis.parse_move s
  >>= Semantic_analysis.process_lex_output
