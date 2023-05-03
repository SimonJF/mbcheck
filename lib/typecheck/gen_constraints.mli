open Common

(* May be convenient to expose expression-level functions. *)

(* Synthesises a type for a given expression and interface environment, also
   returning an environment and constraint set. *)
val synthesise_comp :
    Interface_env.t ->
    Ty_env.t -> (* Declaration environment *)
    Ir.comp ->
    Type.t * Ty_env.t * Constraint_set.t

(* Checks an expression against a type, returning an environment and constraint set.*)
val check_comp :
    Interface_env.t ->
    Ty_env.t ->
    Ir.comp ->
    Type.t ->
    Ty_env.t * Constraint_set.t

(* Check top-level program against a type *)
val synthesise_program :  Ir.program -> Type.t * Ty_env.t * Constraint_set.t

(* Check top-level program against a type. Currently unused. *)
val check_program :  Ir.program -> Type.t -> Ty_env.t * Constraint_set.t

