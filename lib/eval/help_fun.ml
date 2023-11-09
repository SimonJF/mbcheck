open Common.Ir
open Eval_types
open Steps_printer

let step_limit = 20

let global_pid_counter = ref 1

let generate_new_pid () =
  let pid = !global_pid_counter in
  global_pid_counter := !global_pid_counter + 1;
  pid


let mailbox_map : (string,pid) Hashtbl.t = Hashtbl.create 100



let substitute_variables_in_message env message =
  List.map (fun item ->
    match item with
    | Variable (v, _) -> (
        match List.find_opt (fun entry ->
          match entry with
          | ValueEntry (binder, _) when binder.name = v.name -> true
          | InterfaceEntry(binder',_) when binder'.name = v.name -> true
          | _ -> false
        ) env with
        | Some (ValueEntry (_, value)) -> value
        | Some (InterfaceEntry (x,_)) -> Primitive x.name
        | _ -> item
      )
    | _ -> item
  ) message

let interface_name_from_value env value =
  match value with
  | Variable (v1, _) -> (
      match List.find_opt (fun entry ->
        match entry with
        | ValueEntry (binder, _) when binder.name = v1.name -> true
        | _ -> false
      ) env with
      | Some (ValueEntry (_, Primitive v2)) -> [v1.name;v2]
      | None -> [v1.name]
      | _ -> failwith_and_print_buffer ("variable not found in environment: " ^ v1.name ^ string_of_int v1.id)
    )
  | _ -> failwith_and_print_buffer "Expected a variable"  

let find_pid_by_name name =
  match Hashtbl.find_opt mailbox_map name with
  | Some pid -> pid
  | None -> (-1)

let rec add_message_to_mailbox processes target_names message updated_processes current_pid =
  match target_names with
  | [] -> failwith_and_print_buffer "No process found with the given interface names"
  | target_name :: remaining_targets ->
      match processes with
      | [] -> 
          add_message_to_mailbox updated_processes remaining_targets message [] current_pid
      | (prog, pid, steps, inbox, comp, env, cont) as current_process :: rest ->
          let target_pid = find_pid_by_name target_name in
          if pid = target_pid then begin
              Buffer.add_string steps_buffer (Printf.sprintf "\n -> -> Process %d sends a message to Process %d(%s)-> ->\n" current_pid pid target_name);
              let updated_process = (prog, pid, steps, message :: inbox, comp, env, cont) in
              (List.rev (updated_process :: updated_processes) @ rest) end
          else
              add_message_to_mailbox rest target_names message (current_process :: updated_processes) current_pid
        
  
let rec extract_message tag (inbox: inbox) : message * inbox =
  match inbox with
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
        | InterfaceEntry (_, inbox) -> InterfaceEntry (mailbox_binder, inbox)
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


(* env and pid is for transfer mailbox to current process *)
let bind_args_paras args params env current_pid=
  List.map2 (fun arg param ->
    match arg with
    | Pair (Primitive "interface", Primitive name) ->
      let interface_name = 
        match List.find_opt (fun entry ->
          match entry with
          | InterfaceEntry (binder, _) -> Binder.name binder = name
          | _ -> false
        ) env with
        | Some (InterfaceEntry (_, _)) -> Hashtbl.replace mailbox_map name current_pid;name
        | _ -> failwith_and_print_buffer ("Interface name not found in environment: " ^ name)
      in
      ValueEntry (fst param, Primitive interface_name)
    | _ -> ValueEntry (fst param, arg)
  ) args params
  
  
  
(* find the value in envirnment *)
let rec lookup env x =
  match env with
  | [] -> failwith_and_print_buffer "entry not found"
  | entry :: env' ->
    match entry with
    | ValueEntry (y, v) -> 
      if Var.id x = Var.id (Var.of_binder y) then v
      else lookup env' x
    | InterfaceEntry (y,_) -> Pair(Primitive "interface",Primitive y.name)

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