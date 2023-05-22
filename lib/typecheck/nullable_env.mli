(* Operations on nullable type environments.
   A small wrapper over type environments.
   We only need to support intersection between two nullable environments,
   and disjoint combination with a defined environment.
 *)
type t
val intersect : t -> t -> (t * Constraint_set.t)
val combine : Interface_env.t -> Ty_env.t -> t -> (Ty_env.t * Constraint_set.t)
val of_env : Ty_env.t -> t
val null : t
