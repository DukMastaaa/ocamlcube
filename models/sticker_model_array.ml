open! Core
open Bigarray

let face_count = 6

module Constants = struct 
  let up = 0
  let front = 1
  let right = 2
  let back = 3
  let left = 4
  let down = 5
end

let opposite_face face =
  let open Constants in
  if face = up then down
  else if face = front then back
  else if face = right then left
  else if face = back  then front
  else if face = left  then right
  else if face = down  then up
  else -1

let array3_init kind layout dim1 dim2 dim3 f =
  let uninit = Array3.create kind layout dim1 dim2 dim3 in
  for i = 0 to dim1 - 1 do
    for j = 0 to dim2 - 1 do
      for k = 0 to dim3 - 1 do
        uninit.{i, j, k} <- f i j k;
      done
    done
  done;
  uninit

let rotate_clockwise arr2 =
  assert ((Array2.dim1 arr2) = (Array2.dim2 arr2));
  let max = Array2.dim1 arr2 - 1 in
  for i = 0 to max do
    for j = 0 to max do
      let temp = arr2.{i, j} in
      arr2.{i, j}             <- arr2.{max - j, i};
      arr2.{max - j, i}       <- arr2.{max - i, max - j};
      arr2.{max - i, max - j} <- arr2.{j, max - i};
      arr2.{j, max - i}       <- temp;
    done
  done

let rotate_counter_clockwise arr2 =
  assert ((Array2.dim1 arr2) = (Array2.dim2 arr2));
  let max = Array2.dim1 arr2 - 1 in
  for i = 0 to max do
    for j = 0 to max do
      let temp = arr2.{i, j} in
      arr2.{i, j}             <- arr2.{j, max - i};
      arr2.{j, max - i}       <- arr2.{max - i, max - j};
      arr2.{max - i, max - j} <- arr2.{max - j, i};
      arr2.{max - j, i}       <- temp;
    done
  done

type encoded_location =
  | Depth
  | Index
  | Dim_1_depth  (* dim - 1 - depth *)
  | Dim_1_index  (* dim - 1 - index *)

type position = encoded_location * encoded_location

let faces_to_swap =
  let open Constants in
  [|
    [| right; back;  left;  front |];  (* up *)
    [| left;  down;  right; up    |];  (* front *)
    [| down;  back;  up;    front |];  (* right *)
    [| right; down;  left;  up    |];  (* back *)
    [| up;    back;  down;  front |];  (* left *)
    [| left;  back;  right; front |];  (* down*)
  |]

let clockwise_cycles =
  [|
    (* up *)
    [|
      Depth, Index;
      Depth, Index;
      Depth, Index;
      Depth, Index;
    |];
    (* front *)
    [|
      Dim_1_index, Dim_1_depth;
      Depth, Dim_1_index;
      Index, Depth;
      Dim_1_depth, Index;
    |];
    (* right *)
    [|
      Index, Dim_1_depth;
      Dim_1_index, Depth;
      Index, Dim_1_depth;
      Index, Dim_1_depth;
    |];
    (* back *)
    [|
      Index, Dim_1_depth;
      Dim_1_depth, Dim_1_index;
      Dim_1_index, Depth;
      Depth, Index;
    |];
    (* left *)
    [|
      Index, Depth;
      Dim_1_index, Dim_1_depth;
      Index, Depth;
      Index, Depth;
    |];
    (* down *)
    [|
      Dim_1_depth, Index;
      Dim_1_depth, Index;
      Dim_1_depth, Index;
      Dim_1_depth, Index;
    |];
  |]

let counter_clockwise_cycles =
  let temp = Array.copy clockwise_cycles in
  Array.iter ~f:Array.rev_inplace temp;
  temp

let decode_position depth index max (row, col)  =
  let decode_location = function
    | Depth -> depth
    | Index -> index
    | Dim_1_depth -> max - depth
    | Dim_1_index -> max - index in
  decode_location row, decode_location col

let cycle_one arr3 face_cycle instructions decode =
  let init_sticker = decode instructions.(0)
    |> fun (row, col) -> arr3.{face_cycle.(0), row, col} in
  for i = 0 to 2 do
    let row1, col1 = decode instructions.(i) in
    let row2, col2 = decode instructions.(i+1) in
    arr3.{face_cycle.(i), row1, col1} <- arr3.{face_cycle.(i+1), row2, col2};
  done;
  let row_last, col_last = decode instructions.(3) in
  arr3.{face_cycle.(3), row_last, col_last} <- init_sticker

let cycle arr3 face inverse depth =
  let max = Array3.dim2 arr3 - 1 in
  let face_cycle = faces_to_swap.(face) in
  let instructions = (if inverse then counter_clockwise_cycles else clockwise_cycles).(face) in
  for layer = 0 to max do
    for i = 0 to 3 do
      let decode = decode_position depth i max in
      cycle_one arr3 face_cycle instructions decode;
    done
  done

let rotate_face arr3 face inverse =
  let face_slice = Array3.slice_left_2 arr3 face in
  if inverse then rotate_counter_clockwise face_slice
  else rotate_clockwise face_slice

let apply_helper arr3 face depth repeat inverse =
  let Move.Range (start, end_) = depth in
  if start = 0 then rotate_face arr3 face inverse;
  if end_ = Array3.dim2 arr3 - 1 then rotate_face arr3 (opposite_face face) (not inverse);
  for layer = start to end_ do
    cycle arr3 face inverse layer
  done

let apply_move arr3 {Move.depth; action; repeat; inverse} =
  let open Constants in
  let face_turn face = apply_helper arr3 face depth repeat inverse in
  match action with
    | Up -> face_turn up
    | Front -> face_turn front
    | Right -> face_turn right
    | Back -> face_turn back
    | Left -> face_turn left
    | Down -> face_turn down
    (* | SliceM -> apply_helper arr3 right (Range ()) *)
    | _ -> ()

let init dim =
  array3_init Int8_unsigned C_layout face_count dim dim (fun n _ _ -> n)