(* Pre-type checking.
   This is a small, basic pass which is used to rule out basic type errors,
   but primarily exists to annotate guards and variables with interfaces for
   use in the constraint generation phase. *)
open Common
open Common_types
open Ir
open Util.Utility

let pretype_error msg = Errors.Pretype_error msg

module Gripers = struct
    open Format

    let arity_error expected_len actual_len =
        let msg =
            asprintf "Arity error. Expects %d arguments, but %d were provided."
            expected_len actual_len
        in
        raise (pretype_error msg)

    let message_arity_error tag expected_len actual_len =
        let msg =
            asprintf "Arity error. Message '%s' expects %d arguments, but %d were provided."
            tag expected_len actual_len
        in
        raise (pretype_error msg)

    let unbound_variable var =
        let msg =
            asprintf "Unbound variable %s" var
        in
        raise (pretype_error msg)

    let type_mismatch expected actual =
        let msg =
            asprintf "Type mismatch. Expected %a but got %a."
                Pretype.pp expected
                Pretype.pp actual
        in
        raise (pretype_error msg)

    let _type_mismatch_with_message msg expected actual =
        let msg =
            asprintf "%s. Expected %a but got %a."
                msg
                Pretype.pp expected
                Pretype.pp actual
        in
        raise (pretype_error msg)

    let type_mismatch_with_expected msg actual =
        let msg =
            asprintf
                "Type mismatch. Expected %s but got %a."
                msg
                Pretype.pp actual
        in
        raise (pretype_error msg)

    let _empty_guards () =
        raise (pretype_error "Guards cannot be empty")

    let cannot_synth_empty_guards () =
        raise (pretype_error "Need at least one non-fail guard to synthesise the type for a 'guard' expression.")

    let cannot_synth_fail () =
        raise (pretype_error "Cannot synthesise a type for a 'fail' guard")

    let cannot_synth_sum term =
        let msg =
            asprintf
                "Cannot synthesise a type for a sum constructor %a"
                Ir.pp_value term
        in
        raise (pretype_error msg)

end

(* Note: This basically works since we only have mailbox subtyping at present.
 If we were to allow subtyping on other types (e.g., records), we would need
 to expand this. *)
let check_tys expected actual =
    if expected = actual then
        ()
    else
        Gripers.type_mismatch expected actual

module PretypeEnv = struct
    type t = Pretype.t StringMap.t

    let lookup x (env: t) =
        let var_str = Var.unique_name x in
        match StringMap.find_opt var_str env with
            | Some x -> x
            | None -> Gripers.unbound_variable var_str

    let bind x =
        StringMap.add (Var.unique_name x)

    let bind_many =
        List.fold_right (fun (v, prety) acc ->
            StringMap.add (Var.unique_name v) prety acc)

    let from_list xs = bind_many xs StringMap.empty
end

module IEnv = Interface_env

(* We take a bidirectional approach. Unlike in gen_constraints,
   as with most bidirectional systems, we try and synthesise as much
   as we can, since we carry around the type environment with us and
   don't need to preserve as much contextual type information. *)
let rec synthesise_val ienv env value : (value * Pretype.t) =
    match value with
        | VAnnotate (v, ty) ->
            let check_ty = Pretype.of_type ty in
            let v = check_val ienv env v check_ty in
            VAnnotate (v, ty), check_ty
        | Constant c ->
            (Constant c, Pretype.PBase (Constant.type_of c))
        | Variable (x, _) ->
            let ty = PretypeEnv.lookup x env in
            Variable (x, Some ty), ty
        | Primitive prim ->
            (* Look up primitive type from Lib_types *)
            (* The only way something should be parsed as a primitive
               is if its type is present in this map. *)
            let ty =
                List.assoc prim Lib_types.signatures
                |> Pretype.of_type
            in
            (Primitive prim, ty)
        | Pair (v1, v2) ->
            let (v1, ty1) = synthesise_val ienv env v1 in
            let (v2, ty2) = synthesise_val ienv env v2 in
            (Pair (v1, v2), Pretype.PPair (ty1, ty2))
        | Lam { linear; parameters; result_type; body } ->
            (* Defer linearity checking to constraint generation. *)
            let param_types  = List.map snd parameters in
            let pretype_params =
                List.map
                    (fun (b, ty) -> Var.of_binder b, Pretype.of_type ty)
                    parameters
            in
            let result_prety = Pretype.of_type result_type in
            let env = PretypeEnv.bind_many pretype_params env in
            let body = check_comp ienv env body result_prety in
            Lam { linear; parameters; body; result_type },
            Pretype.PFun {
                linear = linear;
                args = param_types;
                result = result_prety
            }
        | ((Inl _) as x) | ((Inr _) as x) -> Gripers.cannot_synth_sum x
and check_val ienv env value ty =
    match value, ty with
        | Inl v, (Pretype.PSum (pty1, _)) ->
            let v = check_val ienv env v pty1 in
            Inl v
        | Inr v, (Pretype.PSum (_, pty2)) ->
            let v = check_val ienv env v pty2 in
            Inr v
        | Inl _, ty | Inr _, ty ->
            raise
                (Gripers.type_mismatch_with_expected
                    "a sum type" ty)
        | _ ->
            let value, inferred_ty = synthesise_val ienv env value in
            check_tys ty inferred_ty;
            value
and synthesise_comp ienv env comp =
    let synth = synthesise_comp ienv env in
    let synthv = synthesise_val ienv env in
    match comp with
        | Annotate (c, ty) ->
            let check_ty = Pretype.of_type ty in
            let c = check_comp ienv env c check_ty in
            Annotate (c, ty), check_ty
        | Return v ->
            let (v, ty) = synthv v in
            Return v, ty
        | New iname ->
            New iname, Pretype.PInterface iname
        | Spawn e ->
            let e =
                check_comp ienv env e (Pretype.PBase Unit)
            in
            Spawn e, Pretype.PBase Unit
        | If { test; then_expr; else_expr } ->
            let test =
                check_val ienv env test (Pretype.PBase Bool)
            in
            let then_expr, ty = synth then_expr in
            let else_expr = check_comp ienv env else_expr ty in
            If { test; then_expr; else_expr }, ty
        | Let { binder; term; cont } ->
            let term, term_ty = synth term in
            let env' = PretypeEnv.bind (Var.of_binder binder) term_ty env in
            let cont, cont_ty = synthesise_comp ienv env' cont in
            Let { binder; term; cont }, cont_ty
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
            Case { term; branch1 = ((bnd1, ty1), e1); branch2 = ((bnd2, ty2), e2) }, e1_ty
        | LetPair { binders = (b1, b2); pair; cont } ->
            let pair, pair_ty = synthv pair in
            let (t1, t2) =
                match pair_ty with
                    | Pretype.PPair (t1, t2) -> (t1, t2)
                    | _ ->
                        raise
                            (Gripers.type_mismatch_with_expected
                             "a pair type" pair_ty)
            in
            let env' =
                env
                |> PretypeEnv.bind (Var.of_binder b1) t1
                |> PretypeEnv.bind (Var.of_binder b2) t2
            in
            let cont, cont_ty = synthesise_comp ienv env' cont in
            LetPair { binders = (b1, b2); pair; cont }, cont_ty
        | Seq (e1, e2) ->
            let e1 = check_comp ienv env e1 (Pretype.PBase Unit) in
            let e2, e2_ty = synth e2 in
            Seq (e1, e2), e2_ty
        | App { func; args } ->
            let open Pretype in
            (* Synthesise type for function; ensure it is a function type *)
            let (func, f_ty) = synthv func in
            let arg_anns, result_ann =
                begin
                    match f_ty with
                        | PFun { args; result; _ } ->
                            List.map Pretype.of_type args, result
                        | t ->
                            Gripers.type_mismatch_with_expected "a function type" t
                end
            in
            (* Basic arity checking *)
            let spec_len = List.length arg_anns in
            let arg_len = List.length args in
            let () =
                if spec_len <> arg_len then
                    Gripers.arity_error spec_len arg_len
            in
            (* Check argument types *)
            let args =
                List.combine args arg_anns
                |> List.map (fun (arg, arg_ty) ->
                    check_val ienv env arg arg_ty)
            in
            (* Synthesise result type *)
            App { func; args }, result_ann
        | Send { target; message = (tag, vals); _ } ->
            let open Pretype in
            (* Typecheck target *)
            let target, target_ty = synthv target in
            (* Ensure target has interface type *)
            begin
                match target_ty with
                    | PInterface iname ->
                        (* Check that:
                            - Message tag is contained within interface
                            - Message payload pretype matches that of the interface *)
                        let payload_target_tys =
                            IEnv.lookup iname ienv
                            |> Interface.lookup tag
                            |> List.map Pretype.of_type
                        in
                        let () =
                            let iface_len = List.length payload_target_tys in
                            let val_len = List.length vals in
                            if val_len <> iface_len then
                                Gripers.message_arity_error tag iface_len val_len
                        in
                        let vals =
                            List.combine vals payload_target_tys
                            |> List.map (fun (e, iface_ty) ->
                                check_val ienv env e iface_ty
                            )
                        in
                        Send {
                            target;
                            message = (tag, vals);
                            iname = Some iname
                         }, Pretype.PBase Unit
                    | ty -> Gripers.type_mismatch_with_expected "an interface type" ty
            end
        | Guard { target; pattern; guards; _ } ->
            let (target, target_ty) = synthv target in
            let iname =
                match target_ty with
                    | PInterface iname -> iname
                    | t -> Gripers.type_mismatch_with_expected "an interface type" t
            in
            (* We can synthesise the type of a guard expression as long as it is
               not a unary 'fail' guard, in which case we need an annotation. *)
            let non_fail_guards =
                List.filter (not << is_fail_guard) guards
            in
            let guards, g_ty =
                match non_fail_guards with
                    | [] ->
                        Gripers.cannot_synth_empty_guards ()
                    | g :: gs ->
                        let g, g_ty = synth_guard ienv env iname g in
                        let gs =
                            List.map (fun g -> check_guard ienv env iname g g_ty) gs
                        in
                        g :: gs, g_ty
            in
            Guard { target; pattern; guards; iname = Some iname }, g_ty
and check_comp ienv env comp ty =
    match comp with
        | Return v ->
            let v = check_val ienv env v ty in
            Return v
        | Guard { target; pattern; guards; _ } when guards = [Fail] ->
            let target, target_ty = synthesise_val ienv env target in
            let iname =
                match target_ty with
                    | PInterface iname -> iname
                    | t -> Gripers.type_mismatch_with_expected "an interface type" t
            in
            Guard { target; pattern; guards = [Fail]; iname = Some iname }
        | _ ->
            let comp, inferred_ty = synthesise_comp ienv env comp in
            check_tys ty inferred_ty;
            comp
and synth_guard ienv env iname g =
    let iface = IEnv.lookup iname ienv in
    match g with
        | Receive { tag; payload_binders; mailbox_binder; cont } ->
            let payload_tys = Interface.lookup tag iface in
            let expected_len = List.length payload_tys in
            (* Arity check *)
            let actual_len = List.length payload_binders in
            let () =
                if expected_len <> actual_len then
                    Gripers.message_arity_error tag expected_len actual_len
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
                    (Pretype.PInterface iname)
            in
            let cont, cont_ty = synthesise_comp ienv env cont in
            Receive { tag; payload_binders; mailbox_binder; cont }, cont_ty
        | Free e ->
            let e, e_ty = synthesise_comp ienv env e in
            Free e, e_ty
        | Fail ->
            Gripers.cannot_synth_fail ()
and check_guard ienv env iname g ty =
    let g, inferred_ty = synth_guard ienv env iname g in
    check_tys ty inferred_ty;
    g

(* Top-level typechecker *)
let check { prog_interfaces; prog_decls; prog_body } =

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
                    args = param_tys;
                    result = Pretype.of_type d.decl_return_type
                })) prog_decls
        |> PretypeEnv.from_list
    in

    (* Checks a declaration *)
    let check_decl d =
        (* Add parameters to environment *)
        let params = param_pretypes d.decl_parameters in
        let env = PretypeEnv.bind_many params decl_env in
        (* Typecheck according to return annotation *)
        let decl_body =
            check_comp ienv env d.decl_body (Pretype.of_type d.decl_return_type)
        in
        { d with decl_body }
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
