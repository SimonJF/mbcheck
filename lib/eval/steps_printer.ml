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
  | Variable (x, _) -> Var.name x
  | _ -> "Other value\n\n\n"
  
(* Convert a frame to its string representation. *)
let show_frame (Frame (binder, comp)) =
  print_pretty (Printf.sprintf "Frame") 4;
  Printf.sprintf "Frame(%s, %s)" (Binder.name binder) (show_comp comp)

(* Convert a frame stack to its string representation. *)
let show_frame_stack sigma =
  let frames = List.map show_frame sigma in
  "[" ^ (String.concat "; " frames) ^ "]"


(* Print the current configuration. *)
let print_config (comp, sigma) =
  counter := !counter + 1; 
  print_pretty (Printf.sprintf "\n------------------- step %d --------------------\n" !counter) 2;
  print_pretty (Printf.sprintf "Comp: ") 3;
  Printf.printf "%s\n\n" (show_comp comp);
  print_pretty (Printf.sprintf "Frame Stack: ") 3;
  Printf.printf "%s\n" (show_frame_stack sigma)

(* Print a value. *)
let print_value v =
  print_pretty (Printf.sprintf "\n-------------------- Done ---------------------\n") 2;
  print_pretty (Printf.sprintf "Final Result:  ") 1;
  match v with
  | Constant (Int i) -> Printf.printf "%d\n\n" i
  | Variable (x, _) -> Printf.printf "%d\n\n" (Var.id x)
  | _ -> Printf.printf "Other value"
