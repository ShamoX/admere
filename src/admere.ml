(* open Admere_implementation *)

module type AdmereImplementation =
  sig
    val minLevel : int
  end

module Admere =
  functor(I : AdmereImplementation) ->
  struct
    type refine =
      DoRefine (* Tells to the engine to refine the cell *)
      | DontRefine (* Tells to the engine to not refine the cell *)
      | DoUnrefine (* Tells to the engine that this cell could be merged up *)
    ;;

    type 'a grid_element =
      Node of 'a
      | Grid of ('a grid_element array)
    ;;

    let _minLevel = I.minLevel
    ;;

  end
