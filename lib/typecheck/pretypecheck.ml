(* Pre-type checking.
   This is a small, basic pass which is used to rule out basic type errors,
   but primarily exists to annotate guards and variables with interfaces for
   use in the constraint generation phase. *)
open Common
open Common_types
open Ir
open Util.Utility
open Source_code

let pretype_error msg pos_list = Errors.Pretype_error (msg,pos_list) 

module Gripers = struct
    open Format

    let arity_error pos expected_len actual_len =
        let msg =
            asprintf "Arity error. Expects %d arguments, but %d were provided."
                expected_len 
                actual_len
        in
        raise (pretype_error msg [pos])

    let tuple_arity_error pos expected_len actual_len =
        let msg =
            asprintf "Arity error. Tuple deconstructor has %d binders, but tuple has %d components."
                expected_len 
                actual_len
        in
        raise (pretype_error msg [pos])

    let message_arity_error pos tag expected_len actual_len =
        let msg =
            asprintf "Arity error. Message '%s' expects %d arguments, but %d were provided."
                tag expected_len actual_len
        in
        raise (pretype_error msg [pos])

    let unbound_variable pos var =
        let msg =
            asprintf "Unbound variable %s." var
        in
        raise (pretype_error msg [pos])

    let type_mismatch pos_list expected actual =
        let msg =
            asprintf "Type mismatch. Expected %a but got %a."                
                Pretype.pp expected
                Pretype.pp actual
        in
        raise (pretype_error msg pos_list)

    let type_mismatch_with_expected pos expected_msg actual =
        let msg =
            asprintf "Type mismatch. Expected %s but got %a."                
                expected_msg
                Pretype.pp actual
        in
        raise (pretype_error msg [pos])

    let cannot_synth_empty_guards pos () =
        let msg =
            asprintf "Need at least one non-fail guard to synthesise the type for a 'guard' expression." 
        in
        raise (pretype_error msg [pos])

    let cannot_synth_fail pos () =
        let msg =
            asprintf "Cannot synthesise a type for a 'fail' guard."
        in
        raise (pretype_error msg [pos])

    let cannot_synth_sum (term: value) =
        let pos = WithPos.pos term in
        let msg =
            asprintf "Cannot synthesise a type for a sum constructor %a."
                Ir.pp_value term
        in
        raise (pretype_error msg [pos])
end

(* Note: This basically works since we only have mailbox subtyping at present.
 If we were to allow subtyping on other types (e.g., records), we would need
 to expand this. *)
let check_tys pos_list expected actual =
    if expected = actual then
        ()
    else
        Gripers.type_mismatch pos_list expected actual

module PretypeEnv = struct
    type t = Pretype.t StringMap.t

    let lookup pos x (env: t) =
        let var_str = Var.unique_name x in
        match StringMap.find_opt var_str env with
            | Some x -> x
            | None -> Gripers.unbound_variable pos var_str

    let bind x =
        StringMap.add (Var.unique_name x)

    let bind_many =
        List.fold_right (fun (v, prety) acc ->
            StringMap.add (Var.unique_name v) prety acc)

    let from_list xs = bind_many xs StringMap.empty
end

module IEnv = Interface_env

(* Check that typaram contains only typing variables *)
let check_typarams pos typarams =
    List.for_all
      (fun (t : (Type.t[@name "ty"])) ->
        match t with
        TVar _ -> true
        | _ -> Gripers.type_mismatch_with_expected pos
                        "type variable" (Pretype.of_type t))
      typarams

(* Check that all type variables occuring in t are in typarams *)
let check_tvar_in_params pos typarams (t : (Type.t[@name "ty"])) =
    let rec aux (t : (Type.t[@name "ty"])) =
        match t with
        TVar s ->
            if List.mem t typarams
            then true
            else Gripers.unbound_variable pos s
            | Base _ -> true
        | Fun _ -> true (* take care of that later *)
        | Tuple ts ->
            List.for_all aux ts
        | Sum (t1, t2) ->
            (aux t1) && (aux t2)
        | Mailbox _ -> true
    in aux t

(* Check that all type variables occuring in interface are bound *)
let check_iface_is_closed (iface_with_pos : Interface.t WithPos.t) : bool =
    let iface = WithPos.node iface_with_pos in
    let pos = WithPos.pos iface_with_pos in
    let typarams = iface.typarams in
    (check_typarams pos typarams)
    && (List.for_all
        (fun (_, ts) -> List.for_all (check_tvar_in_params pos typarams) ts)
        iface.env)

(* Check that all type variables occuring in declaration are bound *)
let check_decl_is_closed (decl_with_pos : Ir.decl WithPos.t) =
    let decl = WithPos.node decl_with_pos in
    let pos = WithPos.pos decl_with_pos in
    let typarams = decl.typarams in
    let iface_aux pos typarams iface =
        match iface with
        None -> true
        | Some (_, ts) ->
            List.for_all (check_tvar_in_params pos typarams) ts
    in
    (* check that value is type-closed *)
    let rec value_aux (v : Ir.value) : bool =
        let vn = WithPos.node v in
        let pos = WithPos.pos v in
        match vn with
        VAnnotate (v, t) ->
            (check_tvar_in_params pos typarams t)
            && (value_aux v)
        | Atom _ | Constant _ | Primitive _ | Variable _ -> true
        | Tuple vs -> List.for_all value_aux vs
        | Inl v | Inr v -> value_aux v
        | Lam { parameters; result_type; body; _ } ->
            (List.for_all (fun (_, t) -> check_tvar_in_params pos typarams t) parameters)
            && (check_tvar_in_params pos typarams result_type)
            && (comp_aux body)
    (* check that guard is type-closed *)
    and guard_aux (g : Ir.guard) : bool =
        let gn = WithPos.node g in
        match gn with
        Receive { cont; _ } -> comp_aux cont
        | Empty (_, c) -> comp_aux c
        | Fail -> true
    (* check that comp is type-closed *)
    and comp_aux (c : Ir.comp) : bool =
        let cn = WithPos.node c in
        let pos = WithPos.pos c in
        match cn with
        Annotate (c, t) ->
            (check_tvar_in_params pos typarams t)
            && (comp_aux c)
        | Let { term; cont; _ } ->
            (comp_aux term)
            && (comp_aux cont)
        | Seq (c1, c2) ->
            (comp_aux c1)
            && (comp_aux c2)
        | Return v -> value_aux v
        | App { tyargs; func; args; } ->
            (value_aux func)
            && (List.for_all value_aux args)
            && (List.for_all (check_tvar_in_params pos typarams) tyargs)
        | If { then_expr; else_expr; _ } ->
            (comp_aux then_expr)
            && (comp_aux else_expr)
        | LetTuple { tuple; cont; _ } ->
            (value_aux tuple) && (comp_aux cont)
        | Case { term; branch1=((_, t1), c1); branch2=((_, t2), c2); _ } ->
            (value_aux term)
            && (check_tvar_in_params pos typarams t1)
            && (check_tvar_in_params pos typarams t2)
            && (comp_aux c1)
            && (comp_aux c2)
        | New (_, ts) ->
            List.for_all (check_tvar_in_params pos typarams) ts
        | Spawn c ->
            comp_aux c
        | Send { target; iface; _ } ->
            (value_aux target) && (iface_aux pos typarams iface)
        | Free (v, iface) ->
            (value_aux v) && (iface_aux pos typarams iface)
        | Guard { target; guards; iface; _} ->
            (value_aux target)
            && (iface_aux pos typarams iface)
            && (List.for_all guard_aux guards)
    in
    (check_typarams pos typarams)
    && (List.for_all (fun (_, t) -> check_tvar_in_params pos typarams t) decl.decl_parameters)
    && (check_tvar_in_params pos typarams decl.decl_return_type)
    && (comp_aux decl.decl_body)


(* We take a bidirectional approach. Unlike in gen_constraints,
   as with most bidirectional systems, we try and synthesise as much
   as we can, since we carry around the type environment with us and
   don't need to preserve as much contextual type information. *)
let rec synthesise_val ienv env value : (value * Pretype.t) =
    let (v, pos) = WithPos.(node value, pos value) in
    let wrap = WithPos.make ~pos in
    match v with
        | VAnnotate (v, ty) ->
            let check_ty = Pretype.of_type ty in
            let v = check_val ienv env v check_ty in
            wrap (VAnnotate (v, ty)), check_ty
        | Atom a -> wrap (Atom a), Pretype.PBase (Base.Atom)
        | Constant c ->
            wrap (Constant c), Pretype.PBase (Constant.type_of c)
        | Variable (x, _) ->
            let ty = PretypeEnv.lookup pos x env in
            wrap (Variable (x, Some ty)), ty
        | Primitive prim ->
            (* Look up primitive type from Lib_types *)
            (* The only way something should be parsed as a primitive
               is if its type is present in this map. *)
            let ty =
                List.assoc prim Lib_types.signatures
                |> Pretype.of_type
            in
            wrap (Primitive prim), ty
        | Tuple vs ->
            let vs_and_tys = List.map (synthesise_val ienv env) vs in
            let (vs, tys) = List.split vs_and_tys in
            wrap (Tuple vs), Pretype.PTuple tys
        | Lam { linear; parameters; result_type; body } ->
            (* Defer linearity checking to constraint generation. *)
            let param_types  = List.map snd parameters in
            let pretype_params =
                List.map
                    (fun (b, ty) -> Var.of_binder b, Pretype.of_type ty)
                    parameters
            in
            let env = PretypeEnv.bind_many pretype_params env in
            let body = check_comp ienv env body (Pretype.of_type result_type) in
            wrap (Lam { linear; parameters; body; result_type }),
            Pretype.PFun {
                linear = linear;
                typarams = [];
                args = param_types;
                result = result_type 
            }
        | Inl _ | Inr _ -> Gripers.cannot_synth_sum value
and check_val ienv env value ty =
    let (value_node, pos) = WithPos.(node value, pos value) in
    let wrap = WithPos.make ~pos in
    match value_node, ty with
        | Inl v, (Pretype.PSum (pty1, _)) ->
            let v = check_val ienv env v pty1 in
            wrap (Inl v)
        | Inr v, (Pretype.PSum (_, pty2)) ->
            let v = check_val ienv env v pty2 in
            wrap (Inr v)
        | Inl _, ty | Inr _, ty ->
            raise
                (Gripers.type_mismatch_with_expected pos
                    "a sum type" ty)
        | _ ->
            let value, inferred_ty = synthesise_val ienv env value in
            check_tys [pos] ty inferred_ty;
            value
and synthesise_comp ienv env comp =
    let pos = WithPos.pos comp in
    let synth = synthesise_comp ienv env in
    let synthv = synthesise_val ienv env in
    match WithPos.node comp with
        | Annotate (c, ty) ->
            let check_ty = Pretype.of_type ty in
            let c = check_comp ienv env c check_ty in
            WithPos.make ~pos (Annotate (c, ty)), check_ty
        | Return v ->
            let (v, ty) = synthv v in
            WithPos.make ~pos (Return v), ty
        | New iname ->
            WithPos.make ~pos (New iname), Pretype.PInterface iname
        | Spawn e ->
            let e =
                check_comp ienv env e (Pretype.unit)
            in
            WithPos.make ~pos (Spawn e), Pretype.unit
        | If { test; then_expr; else_expr } ->
            let test =
                check_val ienv env test (Pretype.PBase Bool)
            in
            let then_expr, ty = synth then_expr in
            let else_expr = check_comp ienv env else_expr ty in
            WithPos.make ~pos (If { test; then_expr; else_expr }), ty
        | Let { binder; term; cont } ->
            let term, term_ty = synth term in
            let env' = PretypeEnv.bind (Var.of_binder binder) term_ty env in
            let cont, cont_ty = synthesise_comp ienv env' cont in
            WithPos.make ~pos (Let { binder; term; cont }), cont_ty
        | Case { term; branch1 = ((bnd1, ty1), e1); branch2 = ((bnd2, ty2), e2) } ->
            let prety1 = Pretype.of_type ty1 in
            let prety2 = Pretype.of_type ty2 in
            let term =
                check_val ienv env term (Pretype.PSum (prety1, prety2))
            in
            let e1_env = PretypeEnv.bind (Var.of_binder bnd1) prety1 env in
            let e2_env = PretypeEnv.bind (Var.of_binder bnd2) prety2 env in
            let e1, e1_ty = synthesise_comp ienv e1_env e1 in
            let e2 = check_comp ienv e2_env e2 e1_ty in
            WithPos.make ~pos
                (Case { term; branch1 = ((bnd1, ty1), e1); branch2 = ((bnd2, ty2), e2) }), e1_ty
        | LetTuple { binders; tuple; cont } ->
            let bnds = List.map fst binders in
            let tuple, tuple_ty = synthv tuple in
            let tys =
                match tuple_ty with
                    | Pretype.PTuple tys -> tys
                    | _ ->
                        raise
                            (Gripers.type_mismatch_with_expected pos
                             "a tuple type" tuple_ty)
            in
            (* Check arity before we start combining *)
            let () =
                let bnds_len = List.length bnds in
                let tys_len = List.length tys in
                if bnds_len <> tys_len then
                    Gripers.tuple_arity_error pos bnds_len tys_len
            in
            let vars_and_tys = List.combine (List.map Var.of_binder bnds) tys in
            let env' =
                PretypeEnv.bind_many vars_and_tys env
            in
            let binders =
                List.combine bnds tys
                |> List.map (fun (b, t) -> (b, Some t))
            in
            let cont, cont_ty = synthesise_comp ienv env' cont in
            WithPos.make ~pos
                (LetTuple { binders; tuple; cont }), cont_ty
        | Seq (e1, e2) ->
            let e1 = check_comp ienv env e1 (Pretype.unit) in
            let e2, e2_ty = synth e2 in
            WithPos.make ~pos(Seq (e1, e2)), e2_ty
        | App { func; tyargs; args } ->
            let open Pretype in
            (* Synthesise type for function; ensure it is a function type *)
            let (func, f_ty) = synthv func in
            let arg_anns, result_ann =
                begin
                    match f_ty with
                        | PFun { typarams; args; result; _ } ->
                          begin
                          try
                            let args' = List.map (Type_utils.substitute_types typarams tyargs) args in
                            let result' = Type_utils.substitute_types typarams tyargs result in
                            List.map Pretype.of_type args', result'
                          with Invalid_argument _ ->
                            Gripers.arity_error pos (List.length typarams) (List.length tyargs)
                          end
                        | t ->
                            Gripers.type_mismatch_with_expected pos "a function type" t
                end
            in
            (* Basic arity checking *)
            let spec_len = List.length arg_anns in
            let arg_len = List.length args in
            let () =
                if spec_len <> arg_len then
                    Gripers.arity_error pos spec_len  arg_len
            in
            (* Check argument types *)
            let args =
                List.combine args arg_anns
                |> List.map (fun (arg, arg_ty) ->
                    check_val ienv env arg arg_ty)
            in
            (* Synthesise result type *)
            WithPos.make ~pos(App { func; tyargs; args }), Pretype.of_type result_ann
        | Send { target; message = (tag, vals); _ } ->
            let open Pretype in
            (* Typecheck target *)
            let target, target_ty = synthv target in
            (* Ensure target has interface type *)
            begin
                match target_ty with
                    | PInterface (iname, tyargs) ->
                        (* Check that:
                            - Message tag is contained within interface
                            - Message payload pretype matches that of the interface *)
                        let interface_withPos = IEnv.lookup iname ienv [(WithPos.pos comp)] in
                        let typarams = Interface.typarams (WithPos.node interface_withPos) in
                        let payload_target_tys =
                          try
                            WithPos.node interface_withPos
                            |> Interface.lookup ~pos_list:(WithPos.extract_pos_pair interface_withPos comp) tag
                            |> List.map (Type_utils.substitute_types typarams tyargs)
                            |> List.map Pretype.of_type
                          with Invalid_argument _ ->
                            Gripers.arity_error pos (List.length typarams) (List.length tyargs)
                        in
                        let () =
                            let iface_len = List.length payload_target_tys in
                            let val_len = List.length vals in
                            if val_len <> iface_len then
                                Gripers.message_arity_error pos tag iface_len val_len
                        in
                        let vals =
                            List.combine vals payload_target_tys
                            |> List.map (fun (e, iface_ty) ->
                                check_val ienv env e iface_ty
                            )
                        in
                        WithPos.make ~pos(
                        Send {
                            target;
                            message = (tag, vals);
                            iface = Some (iname, tyargs)
                         }), Pretype.unit
                    | ty -> Gripers.type_mismatch_with_expected pos "an interface type" ty
            end
        | Free (v, _) ->
            let (v, v_ty) = synthv v in
            let iface =
                match v_ty with
                    | PInterface iface -> iface
                    | t -> Gripers.type_mismatch_with_expected pos "an interface type" t
            in
            WithPos.make ~pos(Free (v, Some iface)), Pretype.unit
        | Guard { target; pattern; guards; _ } ->
            let (target, target_ty) = synthv target in
            let iface =
                match target_ty with
                    | PInterface iface -> iface
                    | t -> Gripers.type_mismatch_with_expected pos "an interface type" t
            in
            (* We can synthesise the type of a guard expression as long as it is
               not a unary 'fail' guard, in which case we need an annotation. *)
            let non_fail_guards = List.filter (not << is_fail_guard) guards
            in
            let guards, g_ty =
                match non_fail_guards with
                    | [] ->
                        Gripers.cannot_synth_empty_guards pos ()
                    | g :: gs ->
                        let g, g_ty = synth_guard ienv env iface g in
                        let gs =
                            List.map (fun g -> check_guard pos ienv env iface g g_ty) gs
                        in
                        g :: gs, g_ty
            in
            WithPos.make ~pos(Guard { target; pattern; guards; iface = Some iface }), g_ty
and check_comp ienv env comp ty  =
    let pos = WithPos.pos comp in
    match WithPos.node comp with
        | Return v ->
            let v = check_val ienv env v ty in
            WithPos.make ~pos (Return v)
        | Guard { target; pattern; guards; _ } when guards = [(WithPos.make ~pos Fail)] ->
            let target, target_ty = synthesise_val ienv env target in
            let iface =
                match target_ty with
                    | PInterface iface -> iface
                    | t -> Gripers.type_mismatch_with_expected pos "an interface type" t
            in
            WithPos.make ~pos (Guard { target; pattern; guards = [(WithPos.make ~pos Fail)]; iface = Some iface })
        | _ ->
            let comp, inferred_ty = synthesise_comp ienv env comp in
            check_tys [pos] ty inferred_ty;
            comp
and synth_guard ienv env ((iname, typarams) : (string * (Type.t[@name "ty"]) list)) g =
    let interface_withPos = IEnv.lookup iname ienv [(WithPos.pos g)] in
    let iface = WithPos.node interface_withPos in
    let pos = WithPos.pos g in
    match WithPos.node g with
        | Receive { tag; payload_binders; mailbox_binder; cont } ->
            let payload_tys = Interface.lookup ~pos_list:[(WithPos.pos interface_withPos);pos] tag iface in
            let expected_len = List.length payload_tys in
            (* Arity check *)
            let actual_len = List.length payload_binders in
            let () =
                if expected_len <> actual_len then
                    Gripers.message_arity_error pos tag expected_len actual_len
            in

            let payload_entries =
                List.combine
                    (List.map Var.of_binder payload_binders)
                    (List.map Pretype.of_type payload_tys)
            in
            let env =
                env
                |> PretypeEnv.bind_many payload_entries
                |> PretypeEnv.bind
                    (Var.of_binder mailbox_binder)
                    (Pretype.PInterface (iname, typarams))
            in
            let cont, cont_ty = synthesise_comp ienv env cont in
            WithPos.make ~pos (Receive { tag; payload_binders; mailbox_binder; cont }), cont_ty
        | Empty (x, e) ->
            let env = PretypeEnv.bind (Var.of_binder x) (Pretype.PInterface (iname, typarams)) env in
            let e, e_ty = synthesise_comp ienv env e in
            WithPos.make ~pos (Empty (x, e)), e_ty
        | Fail ->
            Gripers.cannot_synth_fail pos ()
and check_guard pos ienv env (iface : string * (Type.t[@name "ty"]) list) g ty =
    let g, inferred_ty = synth_guard ienv env iface g in
    check_tys [pos] ty inferred_ty;
    g

(* Top-level typechecker *)
let check { prog_interfaces; prog_decls; prog_body } =

    (* Check that interface and definitions are type-closed.
       Unbound_variable is raised otherwiser. *)
    let _ = List.for_all check_iface_is_closed prog_interfaces in
    let _ = List.for_all check_decl_is_closed prog_decls in
    (* Construct interface environment from interface list *)
    let ienv = IEnv.from_list prog_interfaces in
    let param_pretypes =
        List.map
            (fun (x, t) -> (Var.of_binder x, Pretype.of_type t))
    in

    (* At the moment, I'm doing the Haskell thing of assuming
       that all declarations can refer to each other.
       The alternative is that we have ML-style lexical scoping
       and explicit mutual blocks, which I might do later. *)
    let decl_env =
        List.map (fun d ->
            let param_tys =
                List.map (snd) d.decl_parameters in
            (Var.of_binder d.decl_name,
                Pretype.PFun {
                    linear = false;
                    typarams = d.typarams;
                    args = param_tys;
                    result = d.decl_return_type
                })) (WithPos.extract_list_node prog_decls)
        |> PretypeEnv.from_list
    in

    (* Checks a declaration *)
    let check_decl d =
        let pos = WithPos.pos d in
        let node = WithPos.node d in
        (* Add parameters to environment *)
        let params = param_pretypes node.decl_parameters in
        let env = PretypeEnv.bind_many params decl_env in
        (* Typecheck according to return annotation *)
        let decl_body =
            check_comp ienv env node.decl_body (Pretype.of_type node.decl_return_type)
        in
        WithPos.make ~pos { node with decl_body }
    in

    let prog_decls = List.map check_decl prog_decls in

    let prog_body, ty =
        match prog_body with
            | Some x ->
                let body, ty = synthesise_comp ienv decl_env x in
                Some body, Some ty
            | None -> None, None
    in
    { prog_interfaces; prog_decls; prog_body }, ty
