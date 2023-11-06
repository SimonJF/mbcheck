open Common.Ir
open Common
open Eval_types
open Steps_printer
open Common.Interface

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

let rec execute (program,pid,steps,mailbox,comp,env,stack) =
  Buffer.add_string steps_buffer (print_config (comp,env,stack,steps,pid,mailbox));
  (* Printf.printf "%s" (print_config (comp,env,stack,steps,pid,mailbox)); *)

  let current_steps = Hashtbl.find_opt step_counts pid |> Option.value ~default:0 in
  Hashtbl.replace step_counts pid (current_steps + 1);

  if steps >= step_limit then
    (Unfinished, (program,pid, steps,mailbox, comp, env, stack))
  else
    match comp,env,stack with
    | Return _,_,[] -> 
      (Finished, (program,pid, steps+1,mailbox, comp, env, []))
    
    | Annotate (term, _),env,stack ->
        execute (program,pid, steps+1,mailbox,term,env,stack)

    | Let {binder; term; cont},env,stack ->
        execute (program,pid,steps+1,mailbox,term,env,(Frame (binder,env,cont)) :: stack)

    | LetPair {binders = (binder1, binder2); pair; cont}, env, stack ->
      (match eval_of_var env pair with
      | Pair (v1, v2) ->
          let env' = ValueEntry (binder1, v1) :: ValueEntry ( binder2, v2) :: env in
          execute (program,pid, steps+1,mailbox,cont, env', stack)
      | _ -> failwith_and_print_buffer "Expected a pair in LetPair")
      
    | Seq (comp1, comp2), env, stack ->
      let (status, (_,_,steps',mailbox',comp1_rest,comp_env,comp_stack)) = execute (program,pid, steps+1,mailbox,comp1, env, stack) in
      let new_comp = match comp1_rest with
          | Return _ -> comp2
          | _ -> Seq (comp1_rest, comp2) 
      in
      (match status with
      | Finished | Unfinished ->
          (execute (program,pid, steps', mailbox,new_comp, comp_env, comp_stack))
      | Spawned _ | MessageToSend _ ->
          (status, (program,pid,steps+1, mailbox',new_comp, comp_env, comp_stack)))

    | Return v, env, Frame (x, env', cont) :: stack ->
        let result = eval_of_var env v in
        execute (program,pid, steps+1,mailbox,cont, ValueEntry ( x, result) :: env', stack) 
      
    | App {func; args}, env, stack -> 
        (match func with
        | Lam {parameters; body; _} -> 
            let new_env = bind_args_paras args parameters @ env in
            execute (program,pid, steps+1,mailbox,body, new_env, stack)
        | Primitive op -> 
            (match op with
            | "print" ->
                let value_to_print = List.hd (eval_args args env) in
                Buffer.add_string result_buffer (show_value value_to_print);
                execute (program,pid, steps+1,mailbox, Return (Constant (Unit)), env, stack)
            | "intToString" ->
              let value_to_print = List.hd (eval_args args env) in
                (match value_to_print with
                  | Constant (Int i) ->
                      let converted_string = string_of_int i in
                      execute (program,pid, steps+1,mailbox, Return (Constant (String converted_string)), env, stack)
                  | _ -> failwith_and_print_buffer "Expected integer argument for intToString primitive")
            | "rand" ->
              let value_to_convert =List.hd (eval_args args env) in
              let _ = Random.self_init () in
                (match value_to_convert with
                  | Constant (Int i) ->
                      let random_int = Random.int i in
                      execute (program,pid, steps+1,mailbox, Return (Constant (Int random_int)), env, stack)
                  | _ -> failwith_and_print_buffer "Expected integer argument for rand primitive")
            | "sleep" ->
              let value_to_convert =List.hd (eval_args args env) in
                (match value_to_convert with
                  | Constant (Int i) ->
                      let _ = Unix.sleep i in
                      execute (program,pid, steps+1,mailbox, Return (Constant (Unit)), env, stack)
                  | _ -> failwith_and_print_buffer "Expected integer argument for sleep primitive")
            | "concat" ->
              let list = eval_args args env in
              let value1 = List.hd list in
              let value2 = List.hd (List.tl list) in
                (match value1, value2 with
                  | Constant (String s1), Constant (String s2) ->
                      let concatenated_string = s1 ^ s2 in
                      execute (program,pid, steps+1,mailbox, Return (Constant (String concatenated_string)), env, stack)
                  | _ -> failwith_and_print_buffer "Expected string arguments for concat primitive")
            | _ ->
              let list = eval_args args env in
              let value1 = List.hd list in
              let value2 = List.hd (List.tl list) in
                execute (program,pid, steps+1,mailbox, Return (Constant (eval_of_op op value1 value2)), env, stack)
            )
        | Variable (func_var, _) ->
          let func_name = Var.name func_var in
          (match find_decl func_name program.prog_decls with
          | Some func_decl ->
              let env' = bind_args_paras (List.map (fun arg -> eval_of_var env arg) args) func_decl.decl_parameters in
              let  (_,(_, _, steps',mailbox', comp_result, _, _)) = execute (program,pid, steps+1,mailbox,func_decl.decl_body, env', []) in
              execute (program,pid, steps',mailbox',comp_result, env, stack)
          | None -> failwith_and_print_buffer ("Function " ^ func_name ^ " not found in prog_decls"))
          | _ -> failwith_and_print_buffer "Unhandled function expression in App")

    | If {test; then_expr; else_expr}, env, stack -> 
        let test_value = eval_of_var env test in
        (match test_value with
        | Constant (Bool true) -> execute (program,pid, steps+1,mailbox, then_expr, env, stack)
        | Constant (Bool false) -> execute (program,pid, steps+1,mailbox, else_expr, env, stack)
        | _ -> failwith_and_print_buffer "Expected boolean value in if expression")

    | Case {term; branch1 = (binder1, _), comp1; branch2 = (binder2, _), comp2}, env, stack ->
      let term_value = eval_of_var env term in
      (match term_value with
      | Inl value ->
          let env' = ValueEntry ( binder1, value) :: env in
          execute (program,pid, steps+1,mailbox, comp1, env', stack)
      | Inr value ->
          let env' = ValueEntry ( binder2, value) :: env in
          execute (program,pid, steps+1,mailbox, comp2, env', stack)
      | _ -> failwith_and_print_buffer "Expected Inl or Inr value in Case expression")

    | New interface_name, env, Frame (x, _ ,cont) :: stack ->
      (match List.find_opt (fun iface -> name iface = interface_name) program.prog_interfaces with
      | Some interface_def -> 
          let env' = InterfaceEntry (x, interface_def) :: env in
          execute (program,pid, steps+1,mailbox, cont, env', stack)
      | None -> failwith_and_print_buffer ("Interface " ^ interface_name ^ " not found"))
    
    | Spawn comp, env, stack ->
      let new_process = (program,generate_new_pid (), 0, [], comp, [], stack) in
        (Spawned new_process, (program, pid , steps+1, mailbox, Return (Constant Unit), env, stack))
    
    | Send {target; message; _}, env, stack ->
      (MessageToSend (target, message), (program, pid, steps+1, mailbox, Return (Constant Unit), env, stack))

    | Guard {target; pattern; guards; _}, env, stack ->
      (match pattern with
        | Type.Pattern.One ->
            (match List.find (function Free _ -> true | _ -> false) guards with
            |  (Free cont) -> 
                let new_env = free_mailbox target env in
                execute (program, pid, steps+1, mailbox, cont, new_env, stack)
            | _ -> failwith "No Free guard matched")
        | _ ->
          match mailbox with
            | messages ->
                let rec match_guards = function
                  | [] -> failwith "No guard matched"
                  | Receive {tag; payload_binders; mailbox_binder; cont} :: rest ->
                      if List.exists (fun (msg_tag, _) -> msg_tag = tag) messages then
                        let message_to_process, new_mailbox = extract_message tag messages in
                        let new_env = bind_env message_to_process mailbox_binder payload_binders env in
                        execute (program, pid, steps+1, new_mailbox, cont, new_env, stack)
                      else
                        match_guards rest
                  | _ :: rest -> match_guards rest
                in
                match_guards guards)

    

    | _ ->  failwith_and_print_buffer "Invalid configuration"

let rec process_scheduling processes max_steps =
  match processes with
  | [] -> ()
  | (prog, pid, steps, mailbox, comp, env, stack) :: rest ->
      let total_steps = match Hashtbl.find_opt step_counts pid with
          | Some count -> count
          | None -> steps
        in
        Hashtbl.replace step_counts pid total_steps;
      if steps >= max_steps then begin
        process_scheduling (rest @ [(prog, pid, 0, mailbox, comp, env, stack)]) max_steps
      end else
        let (execution_status, updated_process) = execute (prog, pid, 0, mailbox, comp, env, stack) in
        match execution_status with
        | Finished -> 
            Buffer.add_string steps_buffer (Printf.sprintf "\n******** Process %d Finished \u{221A} ********\n" pid);
            process_scheduling rest max_steps
        | Unfinished -> process_scheduling (rest @ [updated_process]) max_steps
        | Spawned new_process -> 
            Buffer.add_string steps_buffer (Printf.sprintf "\n******** Process %d generates a new Process ********\n" pid;);
            process_scheduling (new_process :: rest @ [updated_process]) max_steps
        | MessageToSend (target, message) ->
            Buffer.add_string steps_buffer (Printf.sprintf "\n -> -> Process %d sends a message -> ->\n" pid);
            let updated_processes = add_message_to_mailbox (updated_process::rest) target message [] in 
            process_scheduling updated_processes max_steps

let generate program =
  Buffer.add_string steps_buffer (Printf.sprintf "\n=== Reduction steps: ===\n\nProgram: %s\n" (show_program program));
  let initial_process =
    match program.prog_body with
    | Some (App { func = Variable (func_var, _); args }) ->
        let func_name = Var.name func_var in
        (match find_decl func_name program.prog_decls with
        | Some func_decl ->
            let env = bind_args_paras args func_decl.decl_parameters in
            (program, generate_new_pid (), 0, [] , func_decl.decl_body, env, [])
        | None -> failwith_and_print_buffer ("Function " ^ func_name ^ " not found in prog_decls"))
    | Some comp -> (program,generate_new_pid (),0, [], comp, [], [])
    | _ -> (program,generate_new_pid () , 0, [], Return (Constant Unit), [], [])
  in
  process_scheduling [initial_process] step_limit


        







