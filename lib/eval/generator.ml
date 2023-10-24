open Common.Ir
open Common_types
open Steps_printer


let find_decl name decls =
  List.find_opt (fun decl -> Binder.name decl.decl_name = name) decls

let bind_args_paras args params =
  List.map2 (fun arg param -> (Var.of_binder (fst param), arg)) args params

(* find the value in envirnment *)
let rec lookup env x =
  match env with
  | [] -> failwith "Variable not found"
  | (y, v) :: env' -> 
    if Var.id x = Var.id y then v
    else lookup env' x


let eval_of_var env v = 
  match v with
  | Variable (var_name, _) -> lookup env var_name
  | c -> c

let eval_of_op op v1 v2 = 
  match v1, v2 with
  | Constant(Int i1), Constant(Int i2) -> (
    match op with
    | "+" -> Int (i1 + i2)
    | "-" -> Int (i1 - i2)
    | "*" -> Int (i1 * i2)
    | "/" -> if i2 = 0 then failwith "Division by zero" else Int (i1 / i2)
    | "==" -> Bool (i1 == i2)
    | "!=" -> Bool (i1 <> i2)
    | "<" -> Bool (i1 < i2)
    | "<=" -> Bool (i1 <= i2)
    | ">" -> Bool (i1 > i2)
    | ">=" -> Bool (i1 >= i2)
    | _ -> failwith ("Unsupported operation: " ^ op)
  )
  | Constant(Bool b1), Constant(Bool b2) -> (
    match op with
    | "&&" -> Bool (b1 && b2)
    | "||" -> Bool (b1 || b2)
    | _ -> failwith ("Unsupported operation: " ^ op)
  )
  | _ -> failwith "Mismatched types or unsupported operation"


let rec execute (program, comp,env,stack) =
  print_config (comp,env,stack);
  match comp,env,stack with
  | Return v,env,[] -> 
      eval_of_var env v
  
  | Annotate (term, _),env,stack ->
      execute (program, term,env,stack)

  | Let {binder; term; cont},env,stack ->
      execute (program, term,env,(Frame (binder, cont)) :: stack)
    
  | Seq (comp1, comp2),env,stack ->
      let _ = execute (program, comp1,env,stack) in
      execute (program, comp2, env, stack)

  | Return v,env,Frame (x, cont) :: stack ->
      let result = eval_of_var env v in
      execute (program, cont,(Var.of_binder x, result) :: env,stack)

  | App {func; args}, env, stack -> 
      (match func with
      | Lam {parameters; body; _} -> 
          let new_env = bind_args_paras args parameters @ env in
          execute (program, body, new_env, stack)
      | Primitive op -> 
          let val1 = eval_of_var env (List.hd args) in
          let val2 = eval_of_var env (List.hd (List.tl args)) in
          execute (program, Return (Constant (eval_of_op op val1 val2)), env, stack)
      | Variable (func_var, _) ->
        let func_name = Var.name func_var in
        (match find_decl func_name program.prog_decls with
        | Some func_decl ->
            let env' = bind_args_paras (List.map (fun arg -> eval_of_var env arg) args) func_decl.decl_parameters in
            let result = execute (program,func_decl.decl_body, env', []) in
            execute (program, Return result, env, stack)
        | None -> failwith ("Function " ^ func_name ^ " not found in prog_decls"))
        | _ -> failwith "Unhandled function expression in App")

  | If {test; then_expr; else_expr}, env, stack -> 
      let test_value = eval_of_var env test in
      (match test_value with
      | Constant (Bool true) -> execute (program, then_expr, env, stack)
      | Constant (Bool false) -> execute (program, else_expr, env, stack)
      | _ -> failwith "Expected boolean value in if expression")

  | _ ->  failwith "Invalid configuration"


let generate program =
  Printf.printf "Program: %s\n" (show_program program);
  match program.prog_body with
  | Some (App { func = Variable (func_var, _); args }) ->
      let func_name = Var.name func_var in
      (match find_decl func_name program.prog_decls with
      | Some func_decl ->
          let env = bind_args_paras args func_decl.decl_parameters in
          execute (program, func_decl.decl_body, env, []) 
      | None -> failwith ("Function " ^ func_name ^ " not found in prog_decls"))
  | Some comp -> execute (program,comp, [], [])
  | _ -> failwith "prog_body does not reference a function to execute"






