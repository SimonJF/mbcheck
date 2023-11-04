open Common.Ir
open Common.Interface

type env_entry =
  | ValueEntry of Binder.t * value
  | InterfaceEntry of Binder.t * t

type process = program * pid * steps * mailbox * comp * environment * frame_stack 
and pid = int
and steps = int
and mailbox = message list
and environment = env_entry list
and frame = Frame of Binder.t * environment * comp
and frame_stack = frame list


type execution_status =
  | Finished
  | Unfinished
  | Spawned of process 
  | MessageToSend of value * message


