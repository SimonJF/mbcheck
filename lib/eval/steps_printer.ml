open Common.Ir
open Common_types

(* Convert a value to its string representation. *)
let show_value v =
  match v with
  | Constant (Int i) -> Printf.sprintf "%d" i
  | Variable (x, _) -> Var.name x
  | _ -> "Other value"

(* Convert a frame to its string representation. *)
let show_frame (Frame (binder, comp)) =
  Printf.sprintf "Frame(%s, %s)" (Binder.name binder) (show_comp comp)

(* Convert a frame stack to its string representation. *)
let show_frame_stack sigma =
  let frames = List.map show_frame sigma in
  "[" ^ (String.concat "; " frames) ^ "]"

(* Print the current configuration. *)
let print_config (comp, sigma) =
  Printf.printf "→ Comp: %s\n\n" (show_comp comp);
  Printf.printf "→ Frame Stack: %s\n" (show_frame_stack sigma)

(* Print a value. *)
let print_value v =
  match v with
  | Constant (Int i) -> Printf.printf "%d" i
  | Variable (x, _) -> Printf.printf "%d" (Var.id x)
  | _ -> Printf.printf "Other value"
