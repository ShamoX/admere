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

    (** This function allow to use with AdmereDisplay module *)
    val getColor : t -> Graphics.color
    (** It takes a value and convert it to a colored value to be displayed *)

    (** This function allow to display in text mode a value *)
    val toStr : t -> string
  end
