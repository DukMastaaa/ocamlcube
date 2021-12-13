open Core

type colour =
  | White
  | Green
  | Red
  | Blue
  | Orange
  | Yellow

type corner_sticker = Corner_Sticker of {
    c: colour;
    within: corner;
    attached: edge_sticker
  }
 and edge_sticker = Edge_Sticker of {
    c: colour;
    within: edge;
    attached: centre_sticker * corner_sticker
  }
 and centre_sticker = Centre_Sticker of {
    c: colour;
    within: centre;
    attached: edge_sticker * edge_sticker * edge_sticker * edge_sticker
  }
 and edge   = Edge of edge_sticker * edge_sticker * edge_sticker * edge_sticker
 and corner = Corner of corner_sticker * corner_sticker * corner_sticker 
 and centre = Centre of centre_sticker

type sticker = Corner_Sticker | Edge_Sticker | Centre_Sticker
type cubie   = Edge | Corner | Centre
type face    = centre * edge * edge * edge * edge * corner * corner * corner * corner
type cube    = face * face * face * face * face * face

let () =
  Printf.printf "hello world\n"
