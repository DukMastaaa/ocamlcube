open! Core
open Angstrom

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

(* 
  2-3Rw2'
*)