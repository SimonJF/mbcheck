open Common.Ir
open Common_types
open Steps_printer


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
    | _ -> failwith ("Unsupported operation: " ^ op)
  )
  | Constant(Bool b1), Constant(Bool b2) -> (
    match op with
    | "&&" -> Bool (b1 && b2)
    | "||" -> Bool (b1 || b2)
    | _ -> failwith ("Unsupported operation: " ^ op)
  )
  | _ -> failwith "Mismatched types or unsupported operation"


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


let rec execute (comp,env,stack) =
  print_config (comp,env,stack);
  match comp,env,stack with
  | Return v,_,[] -> 
       v; 
  
  | Annotate (term, _),env,stack ->
      execute (term,env,stack)

  | Let {binder; term; cont},env,stack ->
      execute (term,env,(Frame (binder, cont)) :: stack)

  | Return v,env,Frame (x, cont) :: stack ->
      let result = eval_of_var env v in
      execute (cont,(Var.of_binder x, result) :: env,stack)

  | App {func = Primitive op; args = [v1; v2]}, env, stack -> 
    let val1 = eval_of_var env v1 in
    let val2 = eval_of_var env v2 in
    let result = eval_of_op op val1 val2 in
    execute (Return (Constant (result)), env, stack)
  
  | _ ->  failwith "Invalid configuration"

let find_decl name decls =
  List.find_opt (fun decl -> Binder.name decl.decl_name = name) decls

let bind_args_paras args params =
  List.map2 (fun arg param -> (Var.of_binder (fst param), arg)) args params

let generate program =
  Printf.printf "Program: %s\n" (show_program program);
  match program.prog_body with
  | Some (App { func = Variable (func_var, _); args }) ->
      let func_name = Var.name func_var in
      (match find_decl func_name program.prog_decls with
      | Some func_decl ->
          let env = bind_args_paras args func_decl.decl_parameters in
          execute (func_decl.decl_body, env, [])
      | None -> failwith ("Function " ^ func_name ^ " not found in prog_decls"))
  | Some comp -> execute (comp, [], [])
  | _ -> failwith "prog_body does not reference a function to execute"


  



