open Common.Ir
open Eval_types
open Steps_printer

let step_limit = 20

let global_pid_counter = ref 1

let generate_new_pid () =
  let pid = !global_pid_counter in
  global_pid_counter := !global_pid_counter + 1;
  pid

let interface_name_from_value value = match value with
  | Variable (v, _) -> v.name
  | _ -> failwith_and_print_buffer "Expected a variable"


let rec has_interface_with_name env interface_name = match env with
  | [] -> false
  | InterfaceEntry (v, _) :: _ when v.name = interface_name -> true
  | _ :: rest -> has_interface_with_name rest interface_name


let rec add_message_to_mailbox processes value message updated_processes = 
  match processes with
  | [] -> 
      if updated_processes = [] then
        failwith_and_print_buffer "No process found with the given interface name"
      else
        List.rev updated_processes
  | (prog, pid, steps, mailbox, comp, env, cont) as current_process :: rest ->
      let interface_name = interface_name_from_value value in
      if has_interface_with_name env interface_name then
        let updated_process = (prog, pid, steps, message :: mailbox, comp, env, cont) in
        List.rev (updated_process :: updated_processes) @ rest
      else
        add_message_to_mailbox rest value message (current_process :: updated_processes)
  
let rec extract_message tag (mailbox: mailbox) : message * mailbox =
  match mailbox with
  | [] -> failwith_and_print_buffer "No message with the given tag"
  | (msg_tag, _) as message :: rest ->
      if msg_tag = tag then
        (message, rest)
      else
        let (found_payload, new_mailbox) = extract_message tag rest in
        (found_payload, message :: new_mailbox)

let bind_env msg mailbox_binder payload_binders env =
  match msg with
  | (_, payload) ->
    if List.length payload <> List.length payload_binders then
      failwith_and_print_buffer "Payload does not match the number of binders"
    else
      let bindings = List.combine payload_binders payload in
      let value_entries = List.map (fun (binder, value) -> ValueEntry (binder, value)) bindings in
      let updated_env = List.map (function
        | InterfaceEntry (_, mailbox) -> InterfaceEntry (mailbox_binder, mailbox)
        | other -> other
      ) env 
      in value_entries @ updated_env

let free_mailbox mailbox_binder env =
  match mailbox_binder with
  | Variable (binder, _) ->
      let updated_env = List.filter (function
        | InterfaceEntry (entry_binder, _) -> entry_binder.name <> binder.name
        | _ -> true) env
      in updated_env
  | _ -> failwith_and_print_buffer "Expected a variable for mailbox binder"
      
        
let find_decl name decls =
  List.find_opt (fun decl -> Binder.name decl.decl_name = name) decls

let bind_args_paras args params =
  List.map2 (fun arg param -> ValueEntry (fst param, arg)) args params
  
(* find the value in envirnment *)
let rec lookup env x =
  match env with
  | [] -> failwith_and_print_buffer "Variable not found"
  | entry :: env' ->
    match entry with
    | ValueEntry (y, v) ->
      if Var.id x = Var.id (Var.of_binder y) then v
      else lookup env' x
    | InterfaceEntry _ -> lookup env' x  (* Skip interface entries *)
    

let eval_of_var env v = 
  match v with
  | Variable (var_name, _) -> lookup env var_name
  | c -> c

let eval_args args env =
  List.map (fun arg -> eval_of_var env arg) args
  

let eval_of_op op v1 v2 = 
  match v1, v2 with
  | Constant(Int i1), Constant(Int i2) -> (
    match op with
    | "+" -> Int (i1 + i2)
    | "-" -> Int (i1 - i2)
    | "*" -> Int (i1 * i2)
    | "/" -> if i2 = 0 then failwith_and_print_buffer "Division by zero" else Int (i1 / i2)
    | "==" -> Bool (i1 == i2)
    | "!=" -> Bool (i1 <> i2)
    | "<" -> Bool (i1 < i2)
    | "<=" -> Bool (i1 <= i2)
    | ">" -> Bool (i1 > i2)
    | ">=" -> Bool (i1 >= i2)
    | _ -> failwith_and_print_buffer ("Unsupported operation: " ^ op)
  )
  | Constant(Bool b1), Constant(Bool b2) -> (
    match op with
    | "&&" -> Bool (b1 && b2)
    | "||" -> Bool (b1 || b2)
    | _ -> failwith_and_print_buffer ("Unsupported operation: " ^ op)
  )
  | _ -> failwith_and_print_buffer "Mismatched types or unsupported operation"