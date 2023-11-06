open Common.Ir
open Eval_types
open Common.Interface

let step_counts : (int, int) Hashtbl.t = Hashtbl.create 10

let counter = ref 0

let steps_buffer = Buffer.create 1024
let result_buffer = Buffer.create 1024
let process_buffer = Buffer.create 1024

let steps_buffer_print () = 
  Printf.printf "%s\n\n" (Buffer.contents steps_buffer)

let process_buffer_print () =
  let sorted_steps = 
    Hashtbl.fold (fun pid steps acc -> (pid, steps) :: acc) step_counts [] 
    |> List.sort (fun (pid1, _) (pid2, _) -> compare pid1 pid2)
  in
  List.iter (fun (pid, steps) ->
    let pid_str = if pid = 1 then "Main" else string_of_int pid in
    Printf.printf "Total steps of PID %s: %d\n" pid_str steps
  ) sorted_steps
  

let result_buffer_print () = 
  Printf.printf "\n%s\n\n" (Buffer.contents result_buffer)


let failwith_and_print_buffer msg =
  steps_buffer_print (); 
  process_buffer_print ();
  result_buffer_print ();
  failwith msg

let show_message (tag, values) =
  let values_str = List.map show_value values |> String.concat ", " in
  Printf.sprintf "(%s, [%s])" tag values_str

let show_mailbox mailbox =
  let messages_str = List.map show_message mailbox in
  "[" ^ (String.concat "; " messages_str) ^ "]\n"

(* Convert a value to its string representation. *)
let show_value v =
  match v with
  | Constant (Int i) -> Printf.sprintf "%d" i
  | Constant (Bool b) -> Printf.sprintf "%b" b
  | Constant (String s) -> Printf.sprintf "%s" s
  | Constant (Unit) -> Printf.sprintf "()"
  | Inl v -> Printf.sprintf "Inl %s" (show_value v)
  | Inr _ -> Printf.sprintf "Inr %s" (show_value v)
  | Variable (x, _) -> Var.name x 
  | Pair (v1, v2) -> Printf.sprintf "(%s, %s)" (show_value v1) (show_value v2)
  | _ -> "Other value"

let name_or_id x = 
  let name = Var.name x in
  let id = string_of_int (Var.id x) in
  if name = "" then "temp_" ^ id else name ^ id

let show_env_entry entry =
  match entry with
  | ValueEntry (binder, value) -> Printf.sprintf "%s -> %s" (name_or_id (Var.of_binder binder)) (show_value value)
  | InterfaceEntry (binder, iface) ->
      let iface_str = Format.asprintf "%a" pp iface in
      Printf.sprintf "%s -> %s" (name_or_id (Var.of_binder binder)) iface_str
  
let show_env env =
  let entries = List.map show_env_entry env in
  "[" ^ (String.concat "; " entries) ^ "]"

(* Convert a frame to its string representation. *)
let show_frame (Frame (binder, env ,comp)) =
  Printf.sprintf "Frame(%s,%s,%s)" (Binder.name binder) (show_env env) (show_comp comp)

(* Convert a frame stack to its string representation. *)
let show_frame_stack stack =
  let frames = List.map show_frame stack in
  "[" ^ (String.concat "; " frames) ^ "]"

(* Print the current configuration. *)
let print_config (comp, env, stack, steps, pid,mailbox) =
  counter := !counter + 1;
  let step_str = Printf.sprintf "\n------------------- total step %d --------------------\n" !counter in
  let pid_str = if pid = 1 then "Main" else string_of_int pid in
  let steps_str = Printf.sprintf "PID: %s Steps: %d\n\n" pid_str steps in
  let comp_str = Printf.sprintf "Comp: %s\n\n" (show_comp comp) in
  let env_str = Printf.sprintf "Env: %s\n\n" (show_env env) in
  let frame_stack_str = Printf.sprintf "Frame Stack: %s\n" (show_frame_stack stack) in
  let mailbox_str = Printf.sprintf "Mailbox: %s\n" (show_mailbox mailbox) in
  step_str ^ steps_str ^ mailbox_str^ comp_str ^ env_str ^ frame_stack_str
