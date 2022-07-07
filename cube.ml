(* open! Core

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
 and edge   = Edge of edge_sticker * edge_sticker
 and corner = Corner of corner_sticker * corner_sticker * corner_sticker 
 and centre = Centre of centre_sticker

(* type sticker = Corner_Sticker | Edge_Sticker | Centre_Sticker *)
(* type cubie   = Edge | Corner | Centre *)
type face    = centre * edge * edge * edge * edge * corner * corner * corner * corner
type cube    = face * face * face * face * face * face

let string_of_colour = function
  | White -> "W"
  | Green -> "G"
  | Red -> "R"
  | Blue -> "B"
  | Orange -> "O"
  | Yellow -> "Y"

let make_cube () =
  (* CENTRES *)
  let rec u = Centre u_s
  and f = Centre f_s
  and r = Centre r_s
  and d = Centre d_s
  and b = Centre b_s
  and l = Centre l_s
  (* EDGES *)
  and fu = Edge (fu_f, fu_u)
  and ul = Edge (ul_u, ul_l)
  and bu = Edge (bu_b, bu_u)
  and ur = Edge (ur_u, ur_r)
  and fr = Edge (fr_f, fr_r)
  and fd = Edge (fd_f, fd_d)
  and fl = Edge (fl_f, fl_l)
  and dr = Edge (dr_d, dr_r)
  and br = Edge (br_b, br_r)
  and bd = Edge (bd_b, bd_d)
  and dl = Edge (dl_d, dl_l)
  and bl = Edge (bl_b, bl_l)
  (* CORNERS *)
  and ful = Corner (ful_f, ful_u, ful_l)
  and bul = Corner (bul_b, bul_u, bul_l)
  and bur = Corner (bur_b, bur_u, bur_r)
  and fur = Corner (fur_f, fur_u, fur_r)
  and fdr = Corner (fdr_f, fdr_d, fdr_r)
  and fdl = Corner (fdl_f, fdl_d, fdl_l)
  and bdr = Corner (bdr_b, bdr_d, bdr_r)
  and bdl = Corner (bdl_b, bdl_d, bdl_l)
  (* CENTRE STICKERS *)
  and u_s = Centre_Sticker {c = White;  within = u; attached = fu_u, ul_u, bu_u, ur_u}
  and f_s = Centre_Sticker {c = Green;  within = f; attached = fu_f, fr_f, fd_f, fl_f}
  and r_s = Centre_Sticker {c = Red;    within = r; attached = dr_r, fr_r, ur_r, br_r}
  and d_s = Centre_Sticker {c = Yellow; within = d; attached = dr_d, bd_d, dl_d, fd_d}
  and b_s = Centre_Sticker {c = Blue;   within = b; attached = bl_b, bd_b, br_b, bu_b}
  and l_s = Centre_Sticker {c = Orange; within = l; attached = bl_l, ul_l, fl_l, dl_l}
  (* EDGE STICKERS *)
  (* u face *)
  and fu_u = Edge_Sticker  {c = White;  within = fu; attached = (u_s, ful_u)}
  and ul_u = Edge_Sticker  {c = White;  within = ul; attached = (u_s, bul_u)}
  and bu_u = Edge_Sticker  {c = White;  within = bu; attached = (u_s, bur_u)}
  and ur_u = Edge_Sticker  {c = White;  within = ur; attached = (u_s, fur_u)} 
  (* f face *)
  and fu_f = Edge_Sticker  {c = Green;  within = fu; attached = (f_s, fur_f)}
  and fr_f = Edge_Sticker  {c = Green;  within = fr; attached = (f_s, fdr_f)}
  and fd_f = Edge_Sticker  {c = Green;  within = fd; attached = (f_s, fdl_f)}
  and fl_f = Edge_Sticker  {c = Green;  within = fl; attached = (f_s, ful_f)}
  (* r face *)
  and dr_r = Edge_Sticker  {c = Red;    within = dr; attached = (r_s, fdr_r)}
  and fr_r = Edge_Sticker  {c = Red;    within = fr; attached = (r_s, fur_r)}
  and ur_r = Edge_Sticker  {c = Red;    within = ur; attached = (r_s, bur_r)}
  and br_r = Edge_Sticker  {c = Red;    within = br; attached = (r_s, bdr_r)}
  (* d face *)
  and dr_d = Edge_Sticker  {c = Yellow; within = dr; attached = (d_s, bdr_d)}
  and bd_d = Edge_Sticker  {c = Yellow; within = bd; attached = (d_s, bdl_d)}
  and dl_d = Edge_Sticker  {c = Yellow; within = dl; attached = (d_s, fdl_d)}
  and fd_d = Edge_Sticker  {c = Yellow; within = fd; attached = (d_s, fdr_d)}
  (* b face *)
  and bl_b = Edge_Sticker  {c = Blue;   within = bl; attached = (b_s, bdl_b)}
  and bd_b = Edge_Sticker  {c = Blue;   within = bd; attached = (b_s, bdr_b)}
  and br_b = Edge_Sticker  {c = Blue;   within = br; attached = (b_s, bur_b)}
  and bu_b = Edge_Sticker  {c = Blue;   within = bu; attached = (b_s, bul_b)}
  (* l face *)
  and bl_l = Edge_Sticker  {c = Orange; within = bl; attached = (l_s, bul_l)}
  and ul_l = Edge_Sticker  {c = Orange; within = ul; attached = (l_s, ful_l)}
  and fl_l = Edge_Sticker  {c = Orange; within = fl; attached = (l_s, fdl_l)}
  and dl_l = Edge_Sticker  {c = Orange; within = dl; attached = (l_s, bdl_l)}
  (* CORNER STICKERS *)
  (* u face *)
  and ful_u = Corner_Sticker {c = White;  within = ful; attached = fu_u}
  and bul_u = Corner_Sticker {c = White;  within = bul; attached = ul_u}
  and bur_u = Corner_Sticker {c = White;  within = bur; attached = bu_u}
  and fur_u = Corner_Sticker {c = White;  within = fur; attached = ur_u}
  (* f face *)
  and fur_f = Corner_Sticker {c = Green;  within = fur; attached = fu_f}
  and fdr_f = Corner_Sticker {c = Green;  within = fdr; attached = fr_f}
  and fdl_f = Corner_Sticker {c = Green;  within = fdl; attached = fd_f}
  and ful_f = Corner_Sticker {c = Green;  within = ful; attached = fl_f}
  (* r face *)
  and fdr_r = Corner_Sticker {c = Red;    within = fdr; attached = dr_r}
  and fur_r = Corner_Sticker {c = Red;    within = fur; attached = fr_r}
  and bur_r = Corner_Sticker {c = Red;    within = bur; attached = ur_r}
  and bdr_r = Corner_Sticker {c = Red;    within = bdr; attached = br_r}
  (* d face *)
  and bdr_d = Corner_Sticker {c = Yellow; within = bdr; attached = dr_d}
  and bdl_d = Corner_Sticker {c = Yellow; within = bdl; attached = bd_d}
  and fdl_d = Corner_Sticker {c = Yellow; within = fdl; attached = dl_d}
  and fdr_d = Corner_Sticker {c = Yellow; within = fdr; attached = fd_d}
  (* b face *)
  and bdl_b = Corner_Sticker {c = Blue;   within = bdl; attached = bl_b}
  and bdr_b = Corner_Sticker {c = Blue;   within = bdr; attached = bd_b}
  and bur_b = Corner_Sticker {c = Blue;   within = bur; attached = br_b}
  and bul_b = Corner_Sticker {c = Blue;   within = bul; attached = bu_b}
  (* l face *)
  and bul_l = Corner_Sticker {c = Orange; within = bul; attached = bl_l}
  and ful_l = Corner_Sticker {c = Orange; within = ful; attached = ul_l}
  and fdl_l = Corner_Sticker {c = Orange; within = fdl; attached = fl_l}
  and bdl_l = Corner_Sticker {c = Orange; within = bdl; attached = dl_l}
  in
  let face_u = u, fu, ul, bu, ur, ful, bul, bur, fur in
  let face_f = f, fu, fr, fd, fl, fur, fdr, fdl, ful in
  let face_r = r, dr, fr, ur, br, fdr, fur, bur, bdr in
  let face_d = d, dr, bd, dl, fd, bdr, bdl, fdl, fdr in
  let face_b = b, bl, bd, br, bu, bdl, bdr, bur, bul in
  let face_l = l, bl, ul, fl, dl, bul, ful, fdl, bdl in
  face_u, face_f, face_r, face_d, face_b, face_l

let print_cube cube =
  (* just gonna figure out how to print U face first *)
  let face_u, _, _, _, _, _ = cube in
  ()

let () =
  Printf.printf "hello world\n";
  let cube = make_cube () in
  print_cube cube; *)
