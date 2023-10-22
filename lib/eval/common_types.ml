type configuration = Common.Ir.comp * environment * frame_stack
and environment = (Common.Ir.Binder.t * Common.Ir.value) list
and frame_stack = frame list
and frame = Frame of Common.Ir.Binder.t * Common.Ir.comp
