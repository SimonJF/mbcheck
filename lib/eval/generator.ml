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
  | [] -> failwith_and_print_buffer "Variable not found"
  | (y, v) :: env' -> 
    if Var.id x = Var.id y then v
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

let rec execute (program,comp,env,stack) =
  Buffer.add_string steps_buffer (print_config (comp,env,stack));
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

  | Return v, env, Frame (x, cont) :: stack ->
    (match v with
    | Inl value -> execute (program, (Return value), env, Frame (x, cont) :: stack)
    | Inr value -> execute (program, (Return value), env, Frame (x, cont) :: stack)
    | _ ->
        let result = eval_of_var env v in
        execute (program, cont, (Var.of_binder x, result) :: env, stack)
    )
    
  | App {func; args}, env, stack -> 
      (match func with
      | Lam {parameters; body; _} -> 
          let new_env = bind_args_paras args parameters @ env in
          execute (program, body, new_env, stack)
      | Primitive op -> 
          (match op with
          | "print" ->
              let value_to_print = List.hd (eval_args args env) in
              Buffer.add_string result_buffer (show_value value_to_print);
              execute (program, Return (Constant (Unit)), env, stack)
          | "intToString" ->
            let value_to_print = List.hd (eval_args args env) in
              (match value_to_print with
                | Constant (Int i) ->
                    let converted_string = string_of_int i in
                    execute (program, Return (Constant (String converted_string)), env, stack)
                | _ -> failwith_and_print_buffer "Expected integer argument for intToString primitive")
          | "rand" ->
            let value_to_convert =List.hd (eval_args args env) in
            let _ = Random.self_init () in
              (match value_to_convert with
                | Constant (Int i) ->
                    let random_int = Random.int i in
                    execute (program, Return (Constant (Int random_int)), env, stack)
                | _ -> failwith_and_print_buffer "Expected integer argument for rand primitive")
          | "sleep" ->
            let value_to_convert =List.hd (eval_args args env) in
              (match value_to_convert with
                | Constant (Int i) ->
                    let _ = Unix.sleep i in
                    execute (program, Return (Constant (Unit)), env, stack)
                | _ -> failwith_and_print_buffer "Expected integer argument for sleep primitive")
          | "concat" ->
            let list = eval_args args env in
            let value1 = List.hd list in
            let value2 = List.hd (List.tl list) in
              (match value1, value2 with
                | Constant (String s1), Constant (String s2) ->
                    let concatenated_string = s1 ^ s2 in
                    execute (program, Return (Constant (String concatenated_string)), env, stack)
                | _ -> failwith_and_print_buffer "Expected string arguments for concat primitive")
          | _ ->
            let list = eval_args args env in
            let value1 = List.hd list in
            let value2 = List.hd (List.tl list) in
              execute (program, Return (Constant (eval_of_op op value1 value2)), env, stack)
          )
      | Variable (func_var, _) ->
        let func_name = Var.name func_var in
        (match find_decl func_name program.prog_decls with
        | Some func_decl ->
            let env' = bind_args_paras (List.map (fun arg -> eval_of_var env arg) args) func_decl.decl_parameters in
            let result = execute (program,func_decl.decl_body, env', []) in
            execute (program, Return result, env, stack)
        | None -> failwith_and_print_buffer ("Function " ^ func_name ^ " not found in prog_decls"))
        | _ -> failwith_and_print_buffer "Unhandled function expression in App")

  | If {test; then_expr; else_expr}, env, stack -> 
      let test_value = eval_of_var env test in
      (match test_value with
      | Constant (Bool true) -> execute (program, then_expr, env, stack)
      | Constant (Bool false) -> execute (program, else_expr, env, stack)
      | _ -> failwith_and_print_buffer "Expected boolean value in if expression")
  
  (* | Case {term; branch1; branch2} *)
  (* | New s ,env,stack -> *)
  (* | Spawn comp *)
  (* | Send {target; message;iname;},env,stack -> *)
  (* | Guard {target; pattern; guards; iname} -> *)

  | _ ->  failwith_and_print_buffer "Invalid configuration"


let generate program =
  Buffer.add_string steps_buffer(Printf.sprintf "\n=== Reduction steps: ===\n\nProgram: %s\n" (show_program program); );
  match program.prog_body with
  | Some (App { func = Variable (func_var, _); args }) ->
      let func_name = Var.name func_var in
      (match find_decl func_name program.prog_decls with
      | Some func_decl ->
          let env = bind_args_paras args func_decl.decl_parameters in
          execute (program, func_decl.decl_body, env, []) 
      | None -> failwith_and_print_buffer ("Function " ^ func_name ^ " not found in prog_decls"))
  | Some comp -> execute (program,comp, [], [])
  | _ -> Constant (String "")






