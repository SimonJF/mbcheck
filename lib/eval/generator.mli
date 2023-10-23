val lookup : (Common.Ir.Var.t * Common.Ir.value) list -> Common.Ir.Var.t -> int
val execute :
  Common.Ir.comp * (Common.Ir.Var.t * Common.Ir.value) list *
  Common_types.frame list -> unit
val find_decl_by_name :
  string -> Common.Ir.decl list -> Common.Ir.decl option
val generate : Common.Ir.program -> unit
