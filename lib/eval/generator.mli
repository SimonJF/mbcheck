val subst :
  Common.Ir.value -> Common.Ir.Var.t -> Common.Ir.comp -> Common.Ir.comp
val execute : Common.Ir.comp * Common_types.frame list -> unit
val generate : Common.Ir.program -> unit