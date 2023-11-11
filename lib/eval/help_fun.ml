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
  

let substitute_in_message env target message =
  let substitute_value item =
    match item with
    | Variable (v, _) -> (
        match List.find_opt (fun (binder, _) -> Binder.name binder = v.name) env with
        | Some (_, value) -> value
        | None ->  item
      )
    | _ -> item
  in
  let substituted_target = substitute_value target in
  let substituted_message = List.map substitute_value message in
  (substituted_target, substituted_message)
  
let find_pid_by_name name =
  match Hashtbl.find_opt mailbox_map name with
  | Some pid -> pid
  | None -> -1

let rec add_message_to_mailbox processes target_name message updated_processes current_pid =
  match target_name with
    | Mailbox m ->
        (match processes with
        | [] -> failwith_and_print_buffer "Process doesn't find"
        | (prog, pid, steps, inbox, comp, env, cont) as current_process :: rest -> 
            let target_pid = find_pid_by_name m in
            if pid = target_pid then begin
                Buffer.add_string steps_buffer (Printf.sprintf "\n -> -> Process %d send a message to Process %d(%s)-> ->\n" current_pid pid m);
                let updated_process = (prog, pid, steps, message :: inbox, comp, env, cont) in
                (List.rev (updated_process :: updated_processes) @ rest) end
            else
                add_message_to_mailbox rest target_name message (current_process :: updated_processes) current_pid)
    | _ -> failwith_and_print_buffer "Expected a variable"
        
  
let rec extract_message tag (inbox: inbox) : message * inbox =
  match inbox with
  | [] -> failwith_and_print_buffer "No message with the given tag"
  | (msg_tag, _) as message :: rest ->
      if msg_tag = tag then
        (message, rest)
      else
        let (found_payload, new_mailbox) = extract_message tag rest in
        (found_payload, message :: new_mailbox)

(* let bind_env msg payload_binders env =
  match msg with
  | (_, payload) ->
    if List.length payload <> List.length payload_binders then
      failwith_and_print_buffer "Payload does not match the number of binders"
    else
      let bindings = List.combine payload_binders payload in
      bindings @ env *)
        
let bind_env msg payload_binders env target mailbox_binder =
  match msg with
  | (_, payload) ->
    if List.length payload <> List.length payload_binders then
      failwith_and_print_buffer "Payload does not match the number of binders"
    else
      let bindings = List.combine payload_binders payload in
      (match target with
      | Variable (v, _) ->
        (match List.find_opt (fun (b, _) -> Binder.name b = v.name) env with
        | Some (_, value) ->
          (mailbox_binder, value) :: bindings @ env
        | None ->
          failwith_and_print_buffer "Target variable not found in environment")
      | _ -> failwith_and_print_buffer "Expected a variable for target")


let free_mailbox mailbox_binder env =
  match mailbox_binder with
  | Variable (binder, _) ->
      let updated_env = List.filter (function
        | (_, Mailbox m) -> (binder.name) <> m
        | _ -> true) env
      in updated_env
  | _ -> failwith_and_print_buffer "Expected a variable for mailbox binder"
      
let find_decl name decls =
  List.find_opt (fun decl -> Binder.name decl.decl_name = name) decls

let bind_args_paras args params =
  List.map2 (fun arg param -> (fst param, arg)) args params
  
(* find the value in envirnment *)
let rec lookup env x =
  match env with
  | [] -> failwith_and_print_buffer "Variable not found"
  | (y, v) :: env' ->
      if Var.id x = Var.id (Var.of_binder y) then v
      else lookup env' x

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