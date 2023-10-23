open Common.Ir
open Common_types
open Steps_printer


let rec subst v x cont =
  match cont with
  | Return (Variable (var, _)) when Var.id x = Var.id var -> Return v
  | Return other_value -> Return other_value
  | Let { binder; term; cont } ->
      if binder.id = Var.id x then
        Let { binder; term; cont = subst v x cont }
      else
        Let { binder; term = subst v x term; cont = subst v x cont }
  | App { func; args } ->
    let values = List.map (fun arg -> 
      match arg with
      | Variable (var, _) when Var.id x = Var.id var -> v
      | _ -> arg
    ) args in App { func; args = values }
  | _ -> failwith "Invalid substitution"


let rec execute (comp,sigma) =
  print_config (comp,sigma);
  match comp,sigma with
  | Return v,[] -> 
    print_value v; 
  
  | Annotate (term, _), sigma ->
      execute (term,sigma)

  | Let {binder; term; cont},sigma ->
      execute (term,(Frame (binder, cont)) :: sigma)

  | Return v,Frame (x, cont) :: sigma ->
      execute (subst v (Var.of_binder x) cont,sigma)

  | App {func = Primitive op; args = [Constant (Int i1); Constant (Int i2)]}, sigma -> 
    let result = match op with
      | "+" -> i1 + i2
      | "-" -> i1 - i2
      | "*" -> i1 * i2
      | "/" -> if i2 = 0 then failwith "Division by zero" else i1 / i2
      | _ -> failwith ("Unsupported operation: " ^ op)
    in
    execute (Return (Constant (Int result)),sigma)
    
  | _ ->  failwith "Invalid configuration"

(* let rec execute_all_decls decls =
  match decls with
  | [] -> ()
  | decl :: rest ->
      execute (decl.decl_body, []);
      execute_all_decls rest *)

let find_decl_by_name name decls =
  List.find_opt (fun decl -> Binder.name decl.decl_name = name) decls

let generate program =
  Printf.printf "Program: %s\n" (show_program program);
  match program.prog_body with
  | Some (App {func = Variable (main1, _); args = []}) ->
      let main_name = Var.name main1 in
      (match find_decl_by_name main_name program.prog_decls with
      | Some main_decl -> execute (main_decl.decl_body, [])
      | None -> failwith ("Function " ^ main_name ^ " not found in prog_decls"))
  | _ -> failwith "prog_body does not reference a function to execute"
      

  



