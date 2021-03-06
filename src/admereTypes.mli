(** Those are the public types used in Admere *)

(** Type return when Admere needs to know if a given cell may be
  * refined,
  * unrefined or none of both. *)
type refine =
    DoRefine (* Tells to the engine to refine the cell *)
  | DontRefine (* Tells to the engine to not refine the cell *)
  | DoUnrefine (* Tells to the engine that this cell could be merged up *)


(** This is internal metadata by cell *)
type cellMeta = {
  mutable unrefine : bool;
}
type 'a cell =
    Node of 'a * cellMeta
  | Grid of 'a cell array * cellMeta
;;

