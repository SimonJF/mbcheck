open Common.Ir
open Common_types

let print_pretty tobeformat num=
  match num with
  | 1 -> ANSITerminal.(print_string [Bold; Foreground Green] tobeformat)
  | 2 -> ANSITerminal.(print_string [Bold; Foreground Cyan] tobeformat)
  | 3 -> ANSITerminal.(print_string [Bold; Foreground Yellow] tobeformat)
  | _ -> ANSITerminal.(print_string [Bold; Foreground Magenta] tobeformat)

let counter = ref 0

(* Convert a value to its string representation. *)
let show_value v =
  match v with
  | Constant (Int i) -> Printf.sprintf "%d" i
  | Constant (Bool b) -> Printf.sprintf "%b" b
  | Variable (x, _) -> Var.name x
  | _ -> "Other value\n"

let name_or_id x = 
  let name = Var.name x in
  let id = string_of_int (Var.id x) in
  if name = "" then "temp_" ^ id else name ^ id

let show_env env =
  let bindings = List.map (fun (x, v) -> Printf.sprintf "%s -> %s" (name_or_id x) (show_value v)) env in
  "[" ^ (String.concat "; " bindings) ^ "]"

(* Convert a comp to its string representation. *)
  
(* Convert a frame to its string representation. *)
let show_frame (Frame (binder, comp)) =
  print_pretty (Printf.sprintf "Frame") 4;
  Printf.sprintf "Frame(%s, %s)" (Binder.name binder) (show_comp comp)

(* Convert a frame stack to its string representation. *)
let show_frame_stack sigma =
  let frames = List.map show_frame sigma in
  "[" ^ (String.concat "; " frames) ^ "]"


(* Print the current configuration. *)
let print_config (comp, env, sigma) =
  counter := !counter + 1; 
  print_pretty (Printf.sprintf "\n------------------- step %d --------------------\n" !counter) 2;
  print_pretty (Printf.sprintf "Comp: ") 3;
  Printf.printf "%s\n\n" (show_comp comp);
  print_pretty (Printf.sprintf "Env: ") 3;
  Printf.printf "%s\n\n" (show_env env);
  print_pretty (Printf.sprintf "Frame Stack: ") 3;
  Printf.printf "%s\n" (show_frame_stack sigma)

(* Print a value. *)
let print_value v =
  print_pretty (Printf.sprintf "\n-------------------- Done ---------------------\n") 2;
  print_pretty (Printf.sprintf "Final Result:  ") 1;
  Printf.printf "%s\n\n" (show_value v)
