(* Typing environments, used as an output of the typing algorithm. *)
open Common
open Common_types

type t

(** Empty type environment *)
val empty : t

(** No type environment was constructed. Different to empty typing environment,
     since it will unify with others. Used for fail guards. *)
val none : t

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

(** Iterated join *)
val join_many : Interface_env.t -> t list -> t * Constraint_set.t

(** Merges two branching environments (e.g., if-then-else, cases) *)
val intersect : t -> t -> t * Constraint_set.t

(** Prints environment to standard output *)
val dump : t -> unit

(** Sets all mailbox types to be usable *)
val make_usable : t -> t

(** Sets all mailbox types to be returnable *)
val make_returnable : t -> t

(** Checks to see whether environment contains a variable: if so,
    checks whether that the given type is a subtype of the type in
    the environment.
 *)
val check_type : Interface_env.t -> Ir.Var.t -> Type.t -> t -> Constraint_set.t
