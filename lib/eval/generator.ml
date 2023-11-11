open Common.Ir
open Common
open Eval_types
open Steps_printer
open Common.Interface
open Help_fun
open Checker


let limit = ref 0

let rec execute (program,pid,steps,inbox,comp,env,stack) =
  Buffer.add_string steps_buffer (print_config (comp,env,stack,steps,pid,inbox,mailbox_map));
  (* Printf.printf "%s" (print_config (comp,env,stack,steps,pid,inbox,mailbox_map)); *)

  let current_steps = Hashtbl.find_opt step_counts pid |> Option.value ~default:0 in
  Hashtbl.replace step_counts pid (current_steps + 1);

  if steps >= step_limit then
    (Unfinished, (program,pid, steps,inbox, comp, env, stack))
  else
    match comp,env,stack with
    | Return _,_,[] -> 
      (Finished, (program,pid, steps+1,inbox, comp, env, []))
    
    | Annotate (term, _),env,stack ->
        execute (program,pid, steps+1,inbox,term,env,stack)

    | Let {binder; term; cont},env,stack ->
        execute (program,pid,steps+1,inbox,term,env,(Frame (binder,env,cont)) :: stack)

    | LetPair {binders = (binder1, binder2); pair; cont}, env, stack ->
      (match eval_of_var env pair with
      | Pair (v1, v2) ->
          let env' =  (binder1, v1) ::  ( binder2, v2) :: env in
          execute (program,pid, steps+1,inbox,cont, env', stack)
      | _ -> failwith_and_print_buffer "Expected a pair in LetPair")
      
    | Seq (comp1, comp2), env, stack ->
      let (status, (_, _, steps', inbox', comp1_rest, env', stack')) = execute (program, pid, steps + 1, inbox, comp1, env, stack) in
      let use_env = match comp1 with
        | App _ -> env 
        | _ -> env'  
      in
      (match comp1_rest with
        | Return _ ->
          (match status with
          | Spawned _| MessageToSend _ ->  (status, (program, pid, steps', inbox', comp2, use_env, stack'))
          | _ -> execute (program, pid, steps', inbox', comp2, use_env, stack'))
        | _ ->
          (match status with
            | Blocked -> (Blocked, (program, pid, steps', inbox', Seq (comp1, comp2), use_env, stack'))
            | _ ->(Unfinished, (program, pid, steps', inbox', Seq (comp1_rest, comp2), use_env, stack'))))

    | Return v, env, Frame (x, env', cont) :: stack ->
        let _ = check_and_update_mailboxes comp pid in
        let result = eval_of_var env v in
        execute (program,pid, steps+1,inbox,cont, ( x, result) :: env', stack) 
      
    | App {func; args}, env, stack -> 
        (match func with
        | Lam {parameters; body; _} -> 
            let new_env = (bind_args_paras args parameters) @ env in
            execute (program,pid, steps+1,inbox,body, new_env, stack)
        | Primitive op -> 
            (match op with
            | "print" ->
                let value_to_print = List.hd (eval_args args env) in
                Buffer.add_string result_buffer (show_value value_to_print);
                execute (program,pid, steps+1,inbox, Return (Constant (Unit)), env, stack)
            | "intToString" ->
              let value_to_print = List.hd (eval_args args env) in
                (match value_to_print with
                  | Constant (Int i) ->
                      let converted_string = string_of_int i in
                      execute (program,pid, steps+1,inbox, Return (Constant (String converted_string)), env, stack)
                  | _ -> failwith_and_print_buffer "Expected integer argument for intToString primitive")
            | "rand" ->
              let value_to_convert =List.hd (eval_args args env) in
              let _ = Random.self_init () in
                (match value_to_convert with
                  | Constant (Int i) ->
                      let random_int = Random.int i in
                      execute (program,pid, steps+1,inbox, Return (Constant (Int random_int)), env, stack)
                  | _ -> failwith_and_print_buffer "Expected integer argument for rand primitive")
            | "sleep" ->
              let value_to_convert =List.hd (eval_args args env) in
                (match value_to_convert with
                  | Constant (Int i) ->
                      let _ = Unix.sleep i in
                      execute (program,pid, steps+1,inbox, Return (Constant (Unit)), env, stack)
                  | _ -> failwith_and_print_buffer "Expected integer argument for sleep primitive")
            | "concat" ->
              let list = eval_args args env in
              let value1 = List.hd list in
              let value2 = List.hd (List.tl list) in
                (match value1, value2 with
                  | Constant (String s1), Constant (String s2) ->
                      let concatenated_string = s1 ^ s2 in
                      execute (program,pid, steps+1,inbox, Return (Constant (String concatenated_string)), env, stack)
                  | _ -> failwith_and_print_buffer "Expected string arguments for concat primitive")
            | _ ->
              let list = eval_args args env in
              let value1 = List.hd list in
              let value2 = List.hd (List.tl list) in
                execute (program,pid, steps+1,inbox, Return (Constant (eval_of_op op value1 value2)), env, stack)
            )
        | Variable (func_var, _) ->
          let func_name = Var.name func_var in
          (match find_decl func_name program.prog_decls with
          | Some func_decl ->
              let env' = bind_args_paras (List.map (fun arg -> eval_of_var env arg) args) (func_decl.decl_parameters) in
              execute (program,pid, steps+1,inbox,func_decl.decl_body, env', stack)
          | None ->
            (match List.find_opt (fun v -> match v with
                            | (binder, Lam _) when Binder.name binder = func_name -> true
                            | _ -> false) env with
              | Some (_, Lam {parameters; body; _}) ->
                  let env' = bind_args_paras (List.map (fun arg -> eval_of_var env arg) args) parameters in
                  execute (program,pid, steps+1,inbox,body, env', [])
              | _ -> failwith_and_print_buffer ("Function " ^ func_name ^ " not found in prog_decls or as a closure in env")))
          | _ -> failwith_and_print_buffer "Unhandled function expression in App")

    | If {test; then_expr; else_expr}, env, stack -> 
        let test_value = eval_of_var env test in
        (match test_value with
        | Constant (Bool true) -> execute (program,pid, steps+1,inbox, then_expr, env, stack)
        | Constant (Bool false) -> execute (program,pid, steps+1,inbox, else_expr, env, stack)
        | _ -> failwith_and_print_buffer "Expected boolean value in if expression")

    | Case {term; branch1 = (binder1, _), comp1; branch2 = (binder2, _), comp2}, env, stack ->
      let term_value = eval_of_var env term in
      (match term_value with
      | Inl value ->
          let env' = ( binder1, value) :: env in
          execute (program,pid, steps+1,inbox, comp1, env', stack)
      | Inr value ->
          let env' = ( binder2, value) :: env in
          execute (program,pid, steps+1,inbox, comp2, env', stack)
      | _ -> failwith_and_print_buffer "Expected Inl or Inr value in Case expression")

    | New interface_name, env, Frame (x, _ ,cont) :: stack ->
      (match List.find_opt (fun iface -> name iface = interface_name) program.prog_interfaces with
      | Some _ -> 
          Hashtbl.add mailbox_map (x.name) pid;
          let env' =  (x,Mailbox x.name) :: env in
          execute (program,pid, steps+1,inbox, cont, env', stack)
      | None -> failwith_and_print_buffer ("Interface " ^ interface_name ^ " not found"))
    
    | Spawn comp, env, stack ->
      let new_pid = generate_new_pid () in
      let _ = check_and_update_mailboxes comp new_pid in
      let new_process = (program,new_pid, 0, [], comp, env, []) in
        (Spawned new_process, (program, pid , steps+1, inbox, Return (Constant Unit), env, stack))
    
    | Send {target; message; _}, env, stack ->
      (MessageToSend (target, message), (program, pid, steps+1, inbox, Return (Constant Unit), env, stack))

    | Guard {target; pattern; guards; _}, env, stack ->
      (match pattern with  
        | Type.Pattern.One ->
            (match List.find (function Free _ -> true | _ -> false) guards with
            |  (Free cont) -> 
                let new_env = free_mailbox target env in
                execute (program, pid, steps+1, inbox, cont, new_env, stack)
            | _ -> failwith_and_print_buffer "No Free guard matched")
        | _ ->
          match inbox with
            | messages ->
                let rec match_guards = function
                  | [] -> 
                    if(!limit <= 11) then begin
                      limit := !limit + 1;
                      Buffer.add_string steps_buffer (Printf.sprintf "\n******** Process %d is waiting for messages ********\n" pid);
                      (Blocked, (program,pid, steps,inbox, comp, env, stack))
                    end
                  else failwith_and_print_buffer "No guard matched"
                  | Receive {tag; payload_binders; mailbox_binder; cont} :: rest ->
                      if List.exists (fun (msg_tag, _) -> msg_tag = tag) messages then
                        let message_to_process, new_mailbox = extract_message tag messages in
                        let new_env = bind_env message_to_process payload_binders env target mailbox_binder in
                        execute (program, pid, steps+1, new_mailbox, cont, new_env, stack)
                      else
                        match_guards rest
                  | _ :: rest ->  match_guards rest in
                match_guards guards )

    | _ ->  failwith_and_print_buffer "Invalid configuration"

let rec process_scheduling processes max_steps =
  match processes with
  | [] -> ()
  | (prog, pid, steps, inbox, comp, env, stack) :: rest ->
      let total_steps = match Hashtbl.find_opt step_counts pid with
          | Some count -> count
          | None -> steps
        in
        Hashtbl.replace step_counts pid total_steps;
      if steps >= max_steps then begin
        process_scheduling (rest @ [(prog, pid, 0, inbox, comp, env, stack)]) max_steps
      end else
        let (execution_status, ((_, _, _,_, _, env',_) as updated_process)) = execute (prog, pid, 0, inbox, comp, env, stack) in
        match execution_status with
        | Finished -> 
            Buffer.add_string steps_buffer (Printf.sprintf "\n******** Process %d Finished \u{221A} ********\n" pid);
            process_scheduling rest max_steps
        | Unfinished -> 
              process_scheduling (rest @ [updated_process]) max_steps
        | Spawned new_process -> 
            Buffer.add_string steps_buffer (Printf.sprintf "\n******** Process %d generates a new Process ********\n" pid;);
            process_scheduling (new_process :: [updated_process] @ rest) max_steps
        | MessageToSend (target, ((tag, _) as message)) ->
            let _,messages = message in
            let (substituted_target, substituted_values) = substitute_in_message env' target messages in
            let updated_processes = add_message_to_mailbox (updated_process::rest) substituted_target (tag,substituted_values) [] pid in 
            process_scheduling updated_processes max_steps
        | Blocked ->
              process_scheduling (rest @ [updated_process]) max_steps

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


        







