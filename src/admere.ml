open AdmereTypes
open AdmereImplementation
open AdmereParameter
open AdmereUtils
open Batteries

module Admere =
  functor(I : AdmereImplementation) -> functor(P : AdmereParameter) ->
    struct
      (** This is internal metadata by cell *)
      type cellMeta = {
        mutable unrefine : bool
      }
      type cell =
          Node of I.t * cellMeta
        | Grid of (cell array)
      ;;

      (** This array of vectors allow to explore the grid l-1 of a cell of
       * level l. *)
      let exploringVectors = generateVectors P.dim
      and nbSubcells = Int.pow 2 P.dim;;

      (** constructor for cellMeta *)
      let newMeta () =
        {
          unrefine = false;
        }
      ;;

      (** making 0.0 vector (depending on dimension...) *)
      let vectorOrigin =
        Array.make P.dim 0.0
      ;;


      (** This is the function initializing the grid recursively to the minimum
       * level required. *)
      let rec newGrid cL coor =
        if P.minLevel >= cL then begin
          Node(I.getInitialData coor, newMeta ())
        end else begin
          let subGrid = Array.make nbSubcells
            (Node(I.getInitialData coor, newMeta ())) in
          for i = 0 to nbSubcells-1 do
            let vect = generateCoordinate
              exploringVectors.(i)
              (dxFromLevel (cL+1))
              coor
            in
            subGrid.(i) <- newGrid (cL+1) vect
          done;
          Grid(subGrid)
        end
      ;;
      (** Here are a description of the arguments used in this function :
        * @cL (int) is the current level of refinement on which the cell is.
        * @coor (float array) is the vector of the coordinates of the cell.
        * *)

      (** grid variable. This is it's initialisation of the grid *)
      let grid = newGrid 1 vectorOrigin;;

    end


