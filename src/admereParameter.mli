(** This is a signature for the parameters of a parameter module for Admere *)

module type AdmereParameter =
  sig
    (** This is the minimum level of the grid *)
    val minLevel : int
    (** This is the maximum level of the grid *)
    val maxLevel : int
    (** This is the number of dimension for the grid *)
    val dim : int
  end
