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

  Printf.printf "------------------------------------------\n";
  print_config (comp,sigma);
  match comp,sigma with
  | Return v,[] -> 
    Printf.printf "\n\nFinal Result: ";
    print_value v; 
    Printf.printf "\n\n\n";

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

let generate program =
  Printf.printf "Program: %s\n" (show_program program);
  match program.prog_body with
  | Some comp -> 
      execute (comp, [])
  | None -> 
      failwith "prog_body is empty" 

  



