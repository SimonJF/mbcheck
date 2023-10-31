open Common.Ir
open Common.Interface

type env_entry =
  | ValueEntry of Binder.t * value
  | InterfaceEntry of Binder.t * t

type process = program * steps * comp * environment * frame_stack 
and environment = env_entry list
and frame = Frame of Binder.t * comp
and frame_stack = frame list
and steps = int

type execution_status =
  | Finished
  | Unfinished
  | Spawned of process 

