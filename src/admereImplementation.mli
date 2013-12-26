(** This is the signature a module must have to be able to be AMR compatible. *)
open AdmereTypes

module type AdmereImplementation =
  sig
    (** This is the type of your data *)
    type t

    (** This function tells Admere if the cell *)
    val mayAdjustRefinement : t -> refine

    (** This function is called upon initialisation of the grid. *)
    val getInitialData : float array -> t
    (** It takes the coordinates of the cell to initialise as a vector.
     * Those coordinates are normalized : They start at -1.0 and stops at +1.0.
     * This means that your module may have a normalisation function and an
     * unormalisation function to be able to access correctly to the cells. *)

    (** Function called when refinement is apply to a cell *)
    val doRefine : int -> t -> float array -> t
    (** Here are a description of the arguments passed to this function :
      * - The first argument given is the new cell level (if the splitted cell
      * was level l, this argument is l+1.
      * - Second argument is the content of the parent cell (this will be erased
      * whence the new subCells are made.
      * - Third argument is the relative position of the new cell regarding the
      * center of the refined cell.
      *
      * -> returned value will be use as the cell content for the new cell.
      * *)

    (** Function called when unrefinement is apply to a group of cells *)
    val doUnrefine : int -> (t * float array) array -> t
    (** Here are a description of the arguments passed to this function :
      * - The first is the new cell level (if the merging cells were level l,
      * this argument will be l-1).
      * - Second argument is an array of couple cell content + relative position
      * regarding the center of the futur cell.
      *
      * -> returned value will be use as the cell content of the merge
      * cell
      * *)

    (** This function allow to use with AdmereDisplay module *)
    val getColor : t -> Graphics.color
    (** It takes a value and convert it to a colored value to be displayed *)

    (** This function allow to display in text mode a value *)
    val toStr : t -> string
  end
