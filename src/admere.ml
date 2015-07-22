open AdmereTypes
open AdmereExceptions
open AdmereImplementation
open AdmereParameter
open AdmereUtils
open Batteries

open Graphics

module Admere =
  functor(I : AdmereImplementation) -> functor(P : AdmereParameter) ->
    struct
      type tree = I.t cell ;;

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
      let makeVectorOrigin () = Array.make P.dim 0.0;;
      let vectorOrigin = makeVectorOrigin () ;;

      let subCoordinate cL i coor =
        generateCoordinate P.dim exploringVectors.(i) (dxFromLevel (cL-1)) coor
      ;;

      let rec iterRecValue f cL coor = function
        Node(v,_) -> f cL coor v
        | Grid(cells,_) -> begin
          for i = 0 to nbSubcells-1 do
            let nCoor = subCoordinate cL i coor in
            iterRecValue f (cL+1) nCoor cells.(i)
          done
        end
      ;;

      let coorToStr coor =
        let s = ref (Printf.sprintf "(") in
        for i = 0 to (P.dim-1) do
          s := !s ^ (if i <> 0 then " ," else "") ^ Printf.sprintf "%f" coor.(i);
        done;
        !s ^ ")"
      ;;

      (** This is the function initializing the grid recursively to the minimum
       * level required. *)
      let rec newGrid cL coor =
        Printf.printf "newGrid %d %s\n" cL (coorToStr coor);
        if P.minLevel <= cL then begin
          Node(I.getInitialData coor, newMeta ())
        end else begin
          let subGrid = Array.make nbSubcells
            (Node(I.getInitialData coor, newMeta ())) in
          for i = 0 to nbSubcells-1 do
            let vect = subCoordinate cL i coor in
            subGrid.(i) <- newGrid (cL+1) vect
          done;
          Grid(subGrid, newMeta ())
        end
      ;;
      (** Here are a description of the arguments used in this function :
        * @cL (int) is the current level of refinement on which the cell is.
        * @coor (float array) is the vector of the coordinates of the cell.
        * *)

      (** grid variable. This is it's initialisation of the grid *)
      let grid = newGrid 1 vectorOrigin;;

      let getGrid () = grid
      and convertCellToColor v = I.getColor v;;

      let iter f =
        iterRecValue f 1 vectorOrigin grid
      ;;

      (*** Display section *)
      let gridColor = ref black
      and title = ref "Admere"
      ;;

      let drawPoint displayGrid ml cL coor v =
        let displayCoor = Array.make 2 0
        and mlm_f = float_of_int (if displayGrid then ml-1 else ml-2) in
        (*
        Printf.printf "plotting (%f, %f) for %s\n" coor.(0) coor.(1) (I.toStr v);
        *)
        for i = 0 to min ((Array.length coor)-1) 1 do
          displayCoor.(i) <- int_of_float ((coor.(i) +. 1.) *. (2. ** mlm_f))
        done;
        set_color (I.getColor v);
        if ml = cL then begin (* Only plots a dot *)
          (*
          Printf.printf "\tDisplayed at (%d, %d)\n" displayCoor.(0) displayCoor.(1);
          *)
          plot displayCoor.(0) displayCoor.(1);
          set_color !gridColor;
          (*
          Printf.printf "\tGrid at (%d, %d) x (%d, %d)\n" (displayCoor.(0)-1)
          (displayCoor.(1)-1) 2 2;
          *)
          if displayGrid then draw_rect (displayCoor.(0)-1) (displayCoor.(1)-1) 2 2
        end else (* We must plot a rectangle. *)
          let rHS = (Int.pow 2 (ml-cL)) - 1 in (* rectangle half size *)
          let x = displayCoor.(0) - rHS
          and y = displayCoor.(1) - rHS
          and w = 2*rHS + 1
          and h = 2*rHS + 1 in begin
            (*
            Printf.printf "\tRectangle at (%d, %d) x (%d, %d)\n" x y w h;
            *)
            fill_rect x y (w-1) (h-1);
            set_color !gridColor;
            if displayGrid then draw_rect (x-1) (y-1) (w+1) (h+1)
          end;
        (*
        Pervasives.flush Pervasives.stdout;
        *)
      ;;
      let prepareDisplayGrid displayGrid gc ml =
        let size = if displayGrid then
          (Int.pow 2 ml) + 1
        else
          Int.pow 2 (ml-1)
        in
        set_color gc;
        resize_window size size
      ;;

      let dumpDisplayedGrid name =
        let size = (Int.pow 2 P.maxLevel) + 1 in
        let img = get_image 0 0 size size in
        ()
      ;;



      let drawGrid ?(displayGrid = true) ?(optGridColor) ?(optTitle) () =
        (match optGridColor with Some(c) -> gridColor := c | None -> ());
        (match optTitle with Some(t) -> title := t | None -> ());
        open_graph "";
        clear_graph ();
        set_window_title !title;
        prepareDisplayGrid displayGrid !gridColor P.maxLevel;
        iter (drawPoint displayGrid P.maxLevel);
        dumpDisplayedGrid "toto.bmp"
      ;;


      (*** AMR work section *)
      (** This function explore and do the refinement work *)
      let rec iterRefinement has_changed cL coor = function
        Node(v,m) -> begin
          match I.mayAdjustRefinement v with
            DoRefine -> begin
              has_changed := true;
              if cL < P.maxLevel then begin
                (* Then we can do refinement *)
                let nCells = Array.make nbSubcells (Node(v,m)) in
                for i = 0 to nbSubcells-1 do
                  let relNewCoor =
                    generateRelativeCoordinate P.dim exploringVectors.(i)
                    (dxFromLevel (cL-1)) in
                  nCells.(i) <- Node(
                    I.doRefine (cL+1) v coor relNewCoor,
                    newMeta ())
                done;
                (* recursive on it. This allow to go down in one strait.
                 * It may loop refining, unrefining if Implementation module
                 * fail to provide a safe refinement process. *)
                iterRefinement has_changed cL coor (Grid(nCells, newMeta ()))
              end else
              (Node(v,m), DoRefine)
            end
          | refinement -> (Node(v,m), refinement)
        end
        | Grid(cells, m) -> begin
          let allUnrefine = ref true (* tells us if we can unrefine this cell *)
          and data = ref [] (* This will serve to store  *)
          in
          for i = 0 to nbSubcells-1 do
            let nCoor = subCoordinate cL i coor in
            let (nCell, refinement) = iterRefinement has_changed (cL+1) nCoor cells.(i) in
            cells.(i) <- nCell;
            match refinement with
              DoRefine -> allUnrefine := false
            | DontRefine -> allUnrefine := false
            | DoUnrefine -> data := (accessNodeValue nCell, nCoor):: !data
            (* filling the data parameter to unrefine, if necessary. *)
          done;
          if !allUnrefine then begin
            has_changed := true;
            (* Here we must unrefine this cell *)
            if List.length !data = nbSubcells then
              let v = I.doUnrefine cL coor !data in (* Got value for futur new cell *)
              (* Just in case we check that we are not in a case we could
               * unrefine again... *)
              match I.mayAdjustRefinement v with
                DoRefine -> (Grid(cells, m), DoRefine) (* Refine or not refine,
                that is the question. In this case we assume we shouldn't
                unrefine*)
            | DontRefine -> (Node(v, newMeta ()), DontRefine)
            | DoUnrefine -> (Node(v, newMeta ()), DoUnrefine)
            else (Grid(cells, m), DontRefine) (* here there was a bug probably... *)
          end else
            (Grid(cells, m), DontRefine)
        end
      ;;
      let checkRefinement () =
        let has_changed = ref false in
        ignore (iterRefinement has_changed 1 vectorOrigin grid);
        Pervasives.flush Pervasives.stdout;
        !has_changed
      ;;


    end


