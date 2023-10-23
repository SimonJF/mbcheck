open Common.Ir
open Common_types
open Steps_printer


let eval_of_op op i1 i2 = 
  match op with
    | "+" -> i1 + i2
    | "-" -> i1 - i2
    | "*" -> i1 * i2
    | "/" -> if i2 = 0 then failwith "Division by zero" else i1 / i2
    | "==" -> if i1 = i2 then 1 else 0
    | "<" -> if i1 < i2 then 1 else 0
    | ">" -> if i1 > i2 then 1 else 0
    | "<=" -> if i1 <= i2 then 1 else 0
    | ">=" -> if i1 >= i2 then 1 else 0
    | "!=" -> if i1 <> i2 then 1 else 0
    | "&&" -> if i1 <> 0 && i2 <> 0 then 1 else 0
    | "||" -> if i1 <> 0 || i2 <> 0 then 1 else 0
    | _ -> failwith ("Unsupported operation: " ^ op)


(* find the value in envirnment *)
let rec lookup env x =
  match env with
  | [] -> failwith "Variable not found"
  | (y, v) :: env' -> 
    if Var.id x = Var.id y then
      match v with
      | Constant (Int i) -> i
      | _ -> failwith "Expected integer value"
    else 
      lookup env' x

let eval_of_var env v = 
  match v with
  | Variable (var_name,_) -> lookup env var_name
  | Constant (Int i) -> i
  | _ -> failwith "unexpected variable"


let rec execute (comp,env,stack) =
  print_config (comp,env,stack);
  match comp,env,stack with
  | Return v,_,[] -> 
    print_value v; 
  
  | Annotate (term, _),env,stack ->
      execute (term,env,stack)

  | Let {binder; term; cont},env,stack ->
      execute (term,env,(Frame (binder, cont)) :: stack)

  | Return v,env,Frame (x, cont) :: stack ->
      execute (cont,(Var.of_binder x, v) :: env,stack)

  | App {func = Primitive op; args = [v1; v2]}, env, stack -> 
      let i1 = eval_of_var env v1 in
      let i2 = eval_of_var env v2 in
      let result = eval_of_op op i1 i2 in
      execute (Return (Constant (Int result)), env, stack)
    
  | _ ->  failwith "Invalid configuration"

let find_decl_by_name name decls =
  List.find_opt (fun decl -> Binder.name decl.decl_name = name) decls

let generate program =
  Printf.printf "Program: %s\n" (show_program program);
  match program.prog_body with
  | Some (App {func = Variable (main1, _); args = []}) ->
      let main_name = Var.name main1 in
      (match find_decl_by_name main_name program.prog_decls with
      | Some main_decl -> execute (main_decl.decl_body, [],[])
      | None -> failwith ("Function " ^ main_name ^ " not found in prog_decls"))
  | Some comp -> execute (comp,[],[])
  | _ -> failwith "prog_body does not reference a function to execute"
      

  



