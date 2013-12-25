open AdmereTypes
open AdmereImplementation
open AdmereParameter
open AdmereUtils
open Batteries

open Graphics

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
      let makeVectorOrigin () = Array.make P.dim 0.0;;
      let vectorOrigin = makeVectorOrigin () ;;

      let subCoordinate cL i coor =
        generateCoordinate exploringVectors.(i) (dxFromLevel (cL-1)) coor
      ;;

      let rec iterRecValue f cL coor = function
        Node(v,_) -> f cL coor v
        | Grid(cells) -> begin
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
          Grid(subGrid)
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

      let drawPoint ml cL coor v =
        let displayCoor = Array.make 2 0
        and mlm_f = float_of_int (ml-1) in
        Printf.printf "plotting (%f, %f) for %s\n" coor.(0) coor.(1) (I.toStr v);
        for i = 0 to min ((Array.length coor)-1) 1 do
          displayCoor.(i) <- int_of_float ((coor.(i) +. 1.) *. (2. ** mlm_f))
        done;
        set_color (I.getColor v);
        if ml = cL then begin (* Only plots a dot *)
          Printf.printf "\tDisplayed at (%d, %d)\n" displayCoor.(0) displayCoor.(1);
          Pervasives.flush Pervasives.stdout;
          plot displayCoor.(0) displayCoor.(1)
        end else (* We must plot a rectangle. *)
          let rHS = (Int.pow 2 (ml-cL)) - 1 in (* rectangle half size *)
          let x = displayCoor.(0) - rHS
          and y = displayCoor.(1) - rHS
          and w = 2*rHS + 1
          and h = 2*rHS + 1 in begin
            Printf.printf "\tRectangle at (%d, %d) x (%d, %d)\n" x y w h;
            Pervasives.flush Pervasives.stdout;
            fill_rect x y (w-1) (h-1);
            set_color !gridColor;
            draw_rect (x-1) (y-1) (w+1) (h+1)
          end
      ;;
      let prepareDisplayGrid gc ml =
        let size = (Int.pow 2 ml) + 1 in
        set_color gc;
        resize_window size size
      ;;

      let dumpDisplayedGrid name =
        let size = (Int.pow 2 P.maxLevel) + 1 in
        let img = get_image 0 0 size size in
        ()
      ;;



      let drawGrid ?(optGridColor) ?(optTitle) () =
        (match optGridColor with Some(c) -> gridColor := c | None -> ());
        (match optTitle with Some(t) -> title := t | None -> ());
        open_graph "";
        clear_graph ();
        set_window_title !title;
        prepareDisplayGrid !gridColor P.maxLevel;
        iter (drawPoint P.maxLevel);
        dumpDisplayedGrid "toto.bmp"
      ;;

    end


