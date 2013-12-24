(** This modules contains utilities functions used in the Admere's modules *)

open AdmereExceptions
open Batteries

(** This function generate the sign relatively to the dimension, and the index
 * in the table. This defines the way the sub-tree of a cell is explored.
 * *)
let generateSign dim i =
  Int.pow (-1) (i/(Int.pow 2 (dim-1)))
;;

(** Make reference array to explore the sub-tree *)
let generateVectors dim =
  let res = Array.make (Int.pow 2 dim) (Array.make dim 0.0) in
  for i = 0 to ((Int.pow 2 dim)-1) do
    res.(i) <- Array.make dim 0.0;
    for d = 1 to dim do
      res.(i).(d-1) <- (float_of_int (generateSign d i)) *. 0.5
    done
  done;
  res
;;

(** generate coordinates from a reference vector, a dx and a reference
 * coordinates *)
let generateCoordinate refVec dx coor =
  if Array.length refVec <> Array.length coor then raise AMR_IncoherentArguments;
  let dim = Array.length refVec in
  let res = Array.make dim 0.0 in
  for i = 0 to dim-1 do
    res.(i) <- coor.(i) +. refVec.(i) *. dx
  done;
  res
;;

let dxFromLevel l =
  1. /.
  (float_of_int (Int.pow 2 l))
;;
