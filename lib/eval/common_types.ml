open Common.Ir
open Common.Interface

type env_entry =
  | ValueEntry of Binder.t * value
  | InterfaceEntry of Binder.t * t

type configuration = comp * environment * frame_stack
and environment = env_entry list
and frame = Frame of Binder.t * comp
and frame_stack = frame list

type process = {
  comp: comp;
  env: env_entry list;
  stack: frame list;
  steps: int;  
}

