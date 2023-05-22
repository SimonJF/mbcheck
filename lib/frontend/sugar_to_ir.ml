open Common
open Common_types
open Util.Utility

(* Transforms the sugared AST to the FGCBV IR *)
(* Takes a rather naive approach by assigning each subexpression
 * to a variable. We can likely do some administrative reductions
 * on the fly a little later.
 *)

(* Maps Strings (source-level variables) to IR variables (Ir.Var) *)
type env = { var_env: Ir.Var.t stringmap }
let empty_env = { var_env = StringMap.empty }

let bind_var bnd env = { var_env = StringMap.add (Ir.Binder.name bnd) (Ir.Var.of_binder bnd) (env.var_env) }
let lookup_var key env =
    match StringMap.find_opt key (env.var_env) with
        | Some ty -> ty
        | None ->
            raise (Errors.transform_error ("Unbound variable " ^ key))

let id = fun _ x -> x

let add_name env name =
    let bnd = Ir.Binder.make ~name:name () in
    let env' = bind_var bnd env in
    (bnd, env')

(* f : 'a -> string, where names : 'a list
 * this allows for add_names to accept a list of non-strings,
 * provided that an appropriate function f is given
 * if names : string list, pass (fun x->x) as f
 *
 * add_names env f names = add_names env (fun x->x) (List.map f names)
 * *)
let add_names env f names =
    List.fold_right
    (fun name (bnds, env) ->
        let (bnd, env') = add_name env (f name) in
        (bnd::bnds, env'))
    names
    ([], env)

let rec transform_prog :
    env ->
    Sugar_ast.program ->
        (env -> Ir.program -> Ir.program) -> Ir.program =
            fun env {prog_interfaces;prog_decls;prog_body} k ->
    let (bnds, env') = add_names env (fun d -> d.Sugar_ast.decl_name) prog_decls in
    {
        prog_interfaces;
        prog_decls = List.map (fun (b, d) -> transform_decl env' d b id) (List.combine bnds prog_decls);
        prog_body =
            match prog_body with
            | Some prog_body -> Some (transform_expr env' prog_body id)
            | None -> None
    } |> k env
and transform_decl :
    env ->
    Sugar_ast.decl ->
    Ir.Binder.t ->
        (env -> Ir.decl -> Ir.decl) -> Ir.decl =
            fun env {decl_parameters; decl_return_type; decl_body; _} decl_binder k ->
    let (bnds, env') = add_names env fst decl_parameters in
    {
        decl_name = decl_binder;
        decl_parameters = List.combine bnds (List.map snd decl_parameters);
        decl_return_type;
        decl_body = transform_expr env' decl_body id
    } |> k env
and transform_expr :
    env ->
    Sugar_ast.expr ->
        (env -> Ir.comp -> Ir.comp) -> Ir.comp = fun env x k ->
    match x with
        (* Looks up a term-level variable in the environment,
           returns IR variable *)
        | Var v ->
            let v = lookup_var v env in
            Ir.Return (Ir.Variable (v, None)) |> k env
        | Primitive x -> Ir.Return (Ir.Primitive x) |> k env
        | Constant x ->
            Ir.Return (Ir.Constant x) |> k env
        | Lam {linear; parameters; result_type; body} ->
            let (bnds, env') = add_names env fst parameters in
            Ir.Return (Ir.Lam {
                linear;
                parameters = List.combine bnds (List.map snd parameters);
                result_type;
                body = transform_expr env' body id }) |> k env
        | Annotate (body, annotation) ->
            Ir.Annotate (transform_expr env body id, annotation)
            |> k env
        | Inl e ->
            transform_subterm env e (fun env v -> Ir.Return (Ir.Inl v) |> k env)
        | Inr e ->
            transform_subterm env e (fun env v -> Ir.Return (Ir.Inr v) |> k env)
        (* Note that annotation will have been desugared to subject annotation *)
        | Let {binder; term; body; _} ->
            (* let x = M in N*)
            (* Create an IR variable based on x *)
            let bnd = Ir.Binder.make ~name:binder () in
            (* Transform M under *old* environment *)
            (* The continuation *)
            transform_expr env term
                (fun env c ->
                    (* Bind it in the environment *)
                    let env' = bind_var bnd env in
                    Ir.Let {
                        binder = bnd;
                        term = c;
                        cont = transform_expr env' body k })
        | Pair (e1, e2) ->
            transform_subterm env e1 (fun env v1 ->
            transform_subterm env e2 (fun env v2 ->
                Ir.Return (Ir.Pair (v1, v2)) |> k env))
        | LetPair {binders = (b1, b2); term; cont } ->
            (* let x = M in N*)
            (* Create an IR variable based on x *)
            let bnd1 = Ir.Binder.make ~name:b1 () in
            let bnd2 = Ir.Binder.make ~name:b2 () in
            (* Transform M under *old* environment *)
            (* The continuation *)
            transform_subterm env term
                (fun env v ->
                    (* Bind it in the environment *)
                    let env' =
                        env
                        |> bind_var bnd1
                        |> bind_var bnd2
                    in
                    Ir.LetPair {
                        binders = (bnd1, bnd2);
                        pair = v;
                        cont = transform_expr env' cont k })
        | Case {
            term;
            branch1 = ((bnd1, ty1), comp1);
            branch2 = ((bnd2, ty2), comp2) } ->
            transform_subterm env term (fun env v ->
                let (ir_bnd1, env1) = add_name env bnd1 in
                let (ir_bnd2, env2) = add_name env bnd2 in
                Ir.Case {
                    term = v;
                    branch1 = (ir_bnd1, ty1), (transform_expr env1 comp1 id);
                    branch2 = (ir_bnd2, ty2), (transform_expr env2 comp2 id);
                } |> k env)
        | Seq (e1, e2) ->
            transform_expr env e1 (fun env c1 ->
            match c1 with
                | Ir.Return (Ir.Constant (Constant.Unit)) ->
                    transform_expr env e2 k
                | _ -> Ir.Seq (c1, transform_expr env e2 k))
        | App {func; args} ->
            transform_subterm env func (fun env funcv ->
            transform_list env args (fun argvs ->
                Ir.App { func = funcv; args = argvs }) k)
        | If {test; then_expr; else_expr} ->
                transform_subterm env test (fun env v ->
                Ir.If {
                    test = v;
                    then_expr = transform_expr env then_expr id;
                    else_expr = transform_expr env else_expr id } |> k env)
        | New i -> Ir.New i |> k env
        | Spawn e -> Ir.Spawn (transform_expr env e id) |> k env
        | Send {target; message; iname} ->
            let (tag, payloads) = message in
            transform_subterm env target (fun env pid ->
                transform_list env payloads (fun payload_vs ->
                    Ir.Send {
                        target = pid;
                        message = (tag, payload_vs);
                        iname }) k)
        | Guard {target; pattern; guards; iname} ->
            transform_subterm env target (fun env v ->
                let gs = List.map (fun x -> transform_guard env x) guards in
                Ir.Guard {
                    target = v;
                    pattern;
                    guards = gs;
                    iname
                } |> k env )
        | SugarFree _ | SugarFail (_, _) -> (* shouldn't ever match *)
                raise (Errors.internal_error "sugar_to_ir.ml" "Encountered SugarFree/SugarFail expression during the IR translation stage")

(* Transforms a subterm into an IR computation, naming if necessary. *)
and transform_subterm
    (env: env)
    (x: Sugar_ast.expr)
    (k: env -> Ir.value -> Ir.comp) =
    (* env: current environment
       x: sugared expression
       k: continuation which takes a *value*
     *)
    (* Return type is an IR computation *)
    (* Call transform_expr on the sugared expression.
       Based on the result, either call continuation if the computation is
       trivial (i.e., Return v) -- keeping in mind Return v doesn't exist in
       sugared AST anymore! (Var, Lam, Constant)
     *)
    transform_expr env x (fun env c ->
        match x with
            (* Translate syntactic values directly to avoid a needless
               administrative reduction.
             *)
            | Primitive p -> Ir.Primitive p |> k env
            | Var var ->
                let v = lookup_var var env in
                Ir.Variable (v, None) |> k env
            | Constant c -> Ir.Constant c |> k env
            | Lam {linear; parameters; result_type; body} ->
                let (bnds, env') = add_names env fst parameters in
                Ir.Lam {
                    linear;
                    parameters = List.combine bnds (List.map snd parameters);
                    result_type;
                    body = transform_expr env' body id } |> k env
            (* Other expressions need to be bound to an intermediate variable *)
            | _ ->
                (* Create a new binder *)
                let bnd = Ir.Binder.make () in
                (* Create a new variable from the binder *)
                let var = Ir.Variable ((Ir.Var.of_binder bnd), None) in
                (* Return a 'let' expression with the binder, binding the computation,
                   and apply the continuation to the bound variable *)
                Ir.Let { binder = bnd; term = c; cont = (k env var) })

and transform_guard :
    env ->
    Sugar_ast.guard -> Ir.guard = fun env x ->
    match x with
    | Receive { tag; payload_binders; mailbox_binder; cont } ->
        let (payload_bnds, env) = add_names env (id 1) payload_binders in
        let (mailbox_bnd, env') = add_name env mailbox_binder in
        let cont = transform_expr env' cont id in
        Ir.Receive {
            tag;
            payload_binders = payload_bnds;
            mailbox_binder = mailbox_bnd;
            cont
        }
    | Free e -> Ir.Free (transform_expr env e id)
    (* type will have been expanded into an annotation by this point *)
    | Fail _ -> Ir.Fail

and transform_list :
    env ->
    Sugar_ast.expr list ->
    (Ir.value list -> Ir.comp) ->
        (env -> Ir.comp -> Ir.comp) -> Ir.comp = fun env vals f k ->
    let rec aux env acc vals f k =
        match vals with
        | [] -> f (List.rev acc) |> k env
        | sugar_v :: sugar_vs -> transform_subterm env sugar_v (fun env v ->
            aux env (v::acc) sugar_vs f k) in
    aux env [] vals f k


(* Externally-facing function *)
let transform : Sugar_ast.program -> Ir.program = fun p ->
    transform_prog empty_env p id
