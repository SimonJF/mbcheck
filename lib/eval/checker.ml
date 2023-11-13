open Common.Ir
open Help_fun

let check_and_update_mailboxes expr current_pid =
  match expr with
  | Return Variable(v,_) -> 
    if Hashtbl.mem mailbox_map v.name then (
      Hashtbl.replace mailbox_map v.name current_pid
    )
  | App { func = _; args } ->
    List.iter (fun arg ->  
      match arg with
      | Variable (v, _) -> 
        if Hashtbl.mem mailbox_map v.name then (
          Hashtbl.replace mailbox_map v.name current_pid
        )
      | _ -> () 
    ) args
  | Send { target = _; message = (_, msg_values); iname = _ } ->
    List.iter (fun msg_value ->
      match msg_value with
      | Variable (v, _) -> 
        if Hashtbl.mem mailbox_map v.name then (
          Hashtbl.replace mailbox_map v.name current_pid;
        )
      | _ -> ()
    ) msg_values
  | _ -> () 


  (* | Return _,_,[] -> 
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
    let (status, (_,_,steps',inbox',comp1_rest,env',stack')) = execute (program,pid, steps+1,inbox,comp1, env, stack) in
    let new_comp = match comp1_rest with
        | Return _ -> comp2
        | _ -> Seq (comp1_rest, comp2) 
    in (status, (program,pid, steps', inbox',new_comp, env', stack'))

  | Return v, env, Frame (x, env', cont) :: stack ->
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
        Hashtbl.add mailbox_map (x.name ^ string_of_int x.id) pid;
        let env' =  (x,Mailbox x.name) :: env in
        execute (program,pid, steps+1,inbox, cont, env', stack)
    | None -> failwith_and_print_buffer ("Interface " ^ interface_name ^ " not found"))
  
  | Spawn comp, env, stack ->
    let new_process = (program,generate_new_pid (), 0, [], comp, env, stack) in
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
                  if(!limit <= 5) then begin
                    limit := !limit + 1;
                    Buffer.add_string steps_buffer (Printf.sprintf "\n******** Process %d is waiting for messages ********\n" pid);
                    (Unfinished, (program,pid, steps,inbox, comp, env, stack))
                  end
                else failwith_and_print_buffer "No guard matched"
                | Receive {tag; payload_binders; mailbox_binder; cont} :: rest ->
                    let _ = mailbox_binder in
                    if List.exists (fun (msg_tag, _) -> msg_tag = tag) messages then
                      let message_to_process, new_mailbox = extract_message tag messages in
                      let new_env = bind_env message_to_process payload_binders env in
                      execute (program, pid, steps+1, new_mailbox, cont, new_env, stack)
                    else
                      match_guards rest
                | _ :: rest ->  match_guards rest in
              match_guards guards ) *)
