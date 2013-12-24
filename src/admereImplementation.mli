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
  end
