open Common
open Typecheck

val pipeline : Sugar_ast.program ->
    Sugar_ast.program *
    Pretype.t option * 
    Ir.program *
    Type.t *
    Ty_env.t *
    Constraint_set.t 
