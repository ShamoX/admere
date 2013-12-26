
(** This is raised when vectors used has a different dimension *)
exception AMR_IncoherentArguments

(** This is raised when trying to access a node element when this is an other
 * cell element *)
exception AMR_NotANode
