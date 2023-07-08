(* Typing environments, used as an output of the typing algorithm. *)
open Common

type t

(** Empty type environment *)
val empty : t

val bind : Ir.Var.t -> Type.t -> t -> t
val lookup : Ir.Var.t -> t -> Type.t
val lookup_opt : Ir.Var.t -> t -> Type.t option
val delete : Ir.Var.t -> t -> t
val delete_binder : Ir.Binder.t -> t -> t
val singleton : Ir.Var.t -> Type.t -> t
val bindings : t -> (Ir.Var.t * Type.t) list
val from_list : (Ir.Var.t * Type.t) list -> t
val iter : (Ir.Var.t -> Type.t -> unit) -> t -> unit

(** Disjoint connection of environments (i.e., the + operator on environments) *)
val combine : Interface_env.t -> t -> t-> t * Constraint_set.t

(** Joins two sequential / concurrent environments *)
val join : Interface_env.t -> t -> t -> t * Constraint_set.t

(** Merges two branching environments (e.g., if-then-else, cases) *)
val intersect : t -> t -> t * Constraint_set.t

(** Prints environment to standard output *)
val dump : t -> unit

(** Sets all mailbox types to be usable *)
val make_usable : t -> t

(** Sets all mailbox types to be returnable *)
val make_returnable : t -> t

(** Makes all types in the environment unrestricted *)
val make_unrestricted : t -> Constraint_set.t

(** Checks to see whether environment contains a variable: if so,
    checks whether that the given type is a subtype of the type in
    the environment.
 *)
val check_type : Interface_env.t -> Ir.Var.t -> Type.t -> t -> Constraint_set.t
