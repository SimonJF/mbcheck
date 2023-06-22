(* Constraint generation.
   Given a source AST annotated with pre-types, in the right place, generate a
   set of constraints and return a (full) type and type environment. *)

(* Since our goal is to propagate contextual information to variables, so that
   we can assign them the correct types and generate the correct constraints,
   the type system uses a form of Zeilberger's backwards bidirectional
   typechecking.  The key point is that unlike regular systems, we *check*
   variables, rather than synthesising them. *)

open Util.Utility
open Common
open Common_types
open Ir
open Type_utils

(* Modules *)

module IEnv = Interface_env

(* Bidirectional constraint generation *)

(* Synthesis mode *)
let rec synthesise_val :
    IEnv.t -> Ty_env.t -> Ir.value -> Type.t * Ty_env.t * Constraint_set.t =
        fun ienv decl_env v ->
    match v with
        | VAnnotate (v, ty) ->
            let (env, constrs) = check_val ienv decl_env v ty in
            ty, env, constrs
        | Constant c ->
            Type.Base (Constant.type_of c),
            Ty_env.empty,
            Constraint_set.empty
        | Primitive prim ->
            let ty = List.assoc prim Lib_types.signatures in
            ty, Ty_env.empty, Constraint_set.empty
        (* No harm having this as a synth case as well *)
        | Pair (v1, v2) ->
            let (t1, env1, constrs1) = synthesise_val ienv decl_env v1 in
            let (t2, env2, constrs2) = synthesise_val ienv decl_env v2 in
            let env, constrs3 = Ty_env.combine ienv env1 env2 in
            Type.Pair (t1, t2),
                env,
                Constraint_set.union_many [constrs1; constrs2; constrs3]
        | Variable (_v, None) -> assert false
        | Variable (v, Some ty) ->
            (* Start by checking in the declaration environment *)
            begin
                match Ty_env.lookup_opt v decl_env, ty with
                    | Some decl_ty, _ ->
                        decl_ty, Ty_env.singleton v decl_ty, Constraint_set.empty
                    (* Otherwise resort to pretyping information *)
                    | _, PBase b ->
                        let ty = Type.Base b in
                        ty, Ty_env.singleton v ty, Constraint_set.empty
                    (* In limited circumstances we can use a pretype annotation to
                       synthesise a function *)
                    | _, PFun { linear; args; result = PBase b } ->
                        let ty = Type.function_type linear args (Type.Base b) in
                        ty, Ty_env.singleton v ty, Constraint_set.empty
                    | _, PFun _ ->
                        Gripers.synth_mailbox_function v
                    | _, _ ->
                        Gripers.synth_variable v
            end
        (* In the formalism, this is a checking case
           rather than a synthesis case. However, we can treat it as a synthesis
           case here since we have annotations on arguments and the body (which
           is exactly the same as having a type annotation on an unannotated
           lambda).
         *)
        | Lam { linear; parameters; result_type; body } ->
            let (env, body_constrs) =
                check_comp ienv decl_env body result_type
            in
            (* As usual, annotations form a lower bound on the inferred types. *)
            let parameter_constrs =
                let lookup_type (bnd, _ty) =
                    Ty_env.lookup_opt (Var.of_binder bnd) env
                in
                let (used_parameters, unused_parameters) =
                    List.partition
                        (Option.is_some << lookup_type)
                        parameters
                in
                let used_constrs =
                    (* Get the lists of declared and inferred types for used parameters,
                       and zip them together. *)
                    let inferred_types =
                        List.map (
                            fst >>
                            Var.of_binder >>
                            (flip Ty_env.lookup) env)
                        used_parameters
                    in
                    let declared_types =
                        List.map snd used_parameters
                    in
                    (* Zip together both lists *)
                    let zipped =
                        List.combine inferred_types declared_types
                    in
                    (* Finally, ensure that the declared type is a subtype of
                       the inferred type *)
                    List.fold_left (fun acc (inferred, declared) ->
                        Constraint_set.union acc (subtype ienv declared inferred))
                        Constraint_set.empty
                        zipped
                in
                (* All unused parameters should be unrestricted. *)
                let unused_constrs =
                    List.fold_left (fun acc (_, ty) ->
                        Constraint_set.union acc (make_unrestricted ty)
                    ) Constraint_set.empty unused_parameters in
                Constraint_set.union used_constrs unused_constrs
            in
            (* Hide the parameters in the returned environment *)
            let parameter_tys = List.map snd parameters in
            let env =
                List.fold_left (fun acc x -> Ty_env.delete (fst x |> Var.of_binder) acc)
                env parameters in
            (* If the function is unrestricted, all environment entries required
               to type the body must also be unrestricted. *)
            let unr_constrs =
                if (not linear) then
                    Ty_env.make_unrestricted env
                else Constraint_set.empty
            in
            (* We can only close over variables that are returnable (to avoid issues with aliasing) *)
            let returnable_env = Ty_env.make_returnable env in
            let constrs = Constraint_set.union_many
                [body_constrs; parameter_constrs; unr_constrs]
            in
            let ty = Type.function_type linear parameter_tys result_type in
            (ty, returnable_env, constrs)
        | other -> Gripers.cannot_synthesise_value other
and check_val :
    IEnv.t -> Ty_env.t -> Ir.value -> Type.t -> Ty_env.t * Constraint_set.t =
        fun ienv decl_env v ty ->
    (* Checks consistency between a pretype pair annotation and the check type *)
    let rec check_pretype_consistency ty pty =
        let open Type in
        let open Pretype in
        match ty, pty with
            | Mailbox { interface; _ }, PInterface iname when interface = iname -> ()
            | Base b1, PBase b2 when b1 = b2 -> ()
            | Fun { result = rty; _ }, PFun { result = rpty; _ } ->
                check_pretype_consistency rty rpty
            | Pair (t1, t2), PPair (pt1, pt2) ->
                check_pretype_consistency t1 pt1;
                check_pretype_consistency t2 pt2
            | Sum (t1, t2), PSum (pt1, pt2) ->
                check_pretype_consistency t1 pt1;
                check_pretype_consistency t2 pt2
            | _, _ -> Gripers.pretype_consistency ty pty
    in
    match v with
        (* Crucial case: note that we are *checking* this variable's type, not
           synthesising! This is how we propagate the type information upwards.
           We check the interfaces, and then assign the type. *)
        | Variable (_v, None) -> assert false (* Should not happen after pre-typing. *)
        | Variable (v, Some pty) ->
            check_pretype_consistency ty pty;
            Ty_env.singleton v ty, Constraint_set.empty
        | Inl v ->
            begin
                match ty with
                    | Type.Sum (t1, _) ->
                        check_val ienv decl_env v (Type.make_returnable t1)
                    | _ -> Gripers.expected_sum_type ty
            end
        | Inr v ->
            begin
                match ty with
                    | Type.Sum (_, t2) ->
                        check_val ienv decl_env v (Type.make_returnable t2)
                    | _ -> Gripers.expected_sum_type ty
            end
        | Pair (v1, v2) ->
            let (t1, t2) =
                begin
                    match ty with
                        | Type.Pair (t1, t2) -> (t1, t2)
                        | _ -> Gripers.expected_pair_type ty
                end
            in
            (* We can only construct a pair if its components are returnable. *)
            let (env1, constrs1) = check_val ienv decl_env v1 (Type.make_returnable t1) in
            let (env2, constrs2) = check_val ienv decl_env v2 (Type.make_returnable t2) in
            let env, constrs3 = Ty_env.combine ienv env1 env2 in
            env, Constraint_set.union_many [constrs1; constrs2; constrs3]
        | _ ->
            let synth_ty, synth_env, synth_constrs =
                synthesise_val ienv decl_env v
            in
            let subty_constrs = subtype ienv synth_ty ty in
            synth_env, Constraint_set.union synth_constrs subty_constrs
and synthesise_comp :
    IEnv.t -> Ty_env.t -> Ir.comp -> Type.t * Ty_env.t * Constraint_set.t =
        fun ienv decl_env e ->
    let synth = synthesise_comp ienv decl_env in
    match e with
        | Annotate (e, ty) ->
            (* Mode switch: synthesis --> checking *)
            let env, constrs = check_comp ienv decl_env e ty in
            ty, env, constrs
        | Return v -> synthesise_val ienv decl_env v
        | New interface ->
            let open Type in
            Mailbox {
                capability = Capability.In;
                interface; pattern = Some One;
                (* 'New' always produces a returnable MB type *)
                quasilinearity = Quasilinearity.Returnable;
            },
            Ty_env.empty,
            Constraint_set.empty
        (* Application is a synthesis case, since functions are always annotated. *)
        | App { func; args } ->
            (* Synthesise the type for the function.
               Note that the function will always be annotated. *)
            let fun_ty, fun_env, fun_constrs = synthesise_val ienv decl_env func in
            let arg_tys, result_ty =
                match fun_ty with
                    | Type.Fun { args; result; _ } ->
                        (args, result)
                    | ty -> Gripers.expected_function func ty
            in
            (* Check that all argument types are compatible with the annotation *)
            let zipped = List.combine args arg_tys in
            let (arg_env, arg_constrs) =
                List.fold_right (fun (x, ty) (acc_env, acc_constrs) ->
                    let (arg_env, arg_constrs) = check_val ienv decl_env x ty in
                    (* Note: arguments must have disjoint type environments *)
                    let (env, env_constrs) = Ty_env.combine ienv arg_env acc_env in
                    let constrs =
                        Constraint_set.union_many
                            [arg_constrs; env_constrs; acc_constrs]
                    in
                    (env, constrs))
                zipped (Ty_env.empty, Constraint_set.empty)
            in
            (* No nested evaluation contexts, so we combine function and
               argument environments, expecting disjointness. *)
            let (env, env_constrs) =
                Ty_env.combine ienv fun_env arg_env
            in
            (* Union constraint sets *)
            let constrs =
                Constraint_set.union_many
                [fun_constrs; arg_constrs; env_constrs]
            in
            (result_ty, env, constrs)
        | Send { target; message = (tag, payloads) ; iname } ->
            let open Type in
            (* Option.get safe since interface name will have been filled in
               by pre-type checking *)
            let iname = Option.get iname in
            let payload_types =
                IEnv.lookup iname ienv
                |> Interface.lookup tag in
            (* Check target has correct output type *)
            let target_ty =
                Mailbox {
                    capability = Out;
                    interface = iname;
                    pattern = Some (Message tag);
                    (* 'Send' gives the MB type the least specific
                       quasilinearity (Usable). It can be coerced to Returnable
                       via subtyping later if necessary, but we don't *need*
                       something to be Returnable for us to be able to send on
                       it. *)
                    quasilinearity = Quasilinearity.Usable
                } in
            let (mb_env, mb_constrs) = check_val ienv decl_env target target_ty in
            (* Check arguments have type specified by interface *)
            (* Now that we're TC-ing the IR, this is much more precise since
               intermediate computations will be A-normalised. *)
            let arg_env, arg_constrs =
                List.combine payloads payload_types
                |> List.fold_left (fun (env, constrs) (payload, iface_ty)  ->
                    let (chk_env, chk_constrs) = check_val ienv decl_env payload iface_ty in
                    let (env, env_constrs) = Ty_env.combine ienv env chk_env in
                    (env, Constraint_set.union_many
                        [constrs; chk_constrs; env_constrs])
                ) (Ty_env.empty, Constraint_set.empty)
            in
            let (env, env_constrs) = Ty_env.combine ienv mb_env arg_env in
            let constrs =
                Constraint_set.union_many
                [ mb_constrs; arg_constrs; env_constrs ] in
            (Type.unit_type, env, constrs)
        (* Note: Let in synthesis mode is only useful in certain situations
           because of the lack of contextual type information.

           In essence, this should only really be used when typing a top-level
           'let'. This commonly occurs due to IR conversion.
         *)
        | Let { binder; term; cont = body } ->
            let body_ty, body_env, body_constrs = synth body in
            let binder_var = Var.of_binder binder in
            let env, constrs =
                match Ty_env.lookup_opt (Var.of_binder binder) body_env with
                    | Some binder_ty ->
                        let (term_env, term_constrs) =
                            check_comp ienv decl_env term (Type.make_returnable binder_ty)
                        in
                        (* Join environments, union constraints *)
                        let (env, env_constrs) =
                            Ty_env.join ienv term_env body_env
                        in
                        let constrs =
                            Constraint_set.union_many
                                [body_constrs; term_constrs; env_constrs] in
                        env, constrs
                    | None ->
                        (* In this case, all we can really do is synthesise and
                           check it's not linear. *)
                        let (binder_ty, binder_env, binder_constrs) =
                            synthesise_comp ienv decl_env term in
                        (* The synthesised type must be returnable *)
                        let () =
                            if not (Type.is_returnable binder_ty) then
                                Gripers.let_not_returnable binder_ty
                        in
                        let () =
                            if Type.is_lin binder_ty then
                                Gripers.unused_synthesised_linear_var
                                binder_var binder_ty
                        in
                        let (env, env_constrs) =
                            Ty_env.join ienv binder_env body_env
                        in
                        let constrs =
                            Constraint_set.union_many
                                [body_constrs; binder_constrs; env_constrs]
                        in
                        env, constrs
            in
            body_ty, Ty_env.delete binder_var env, constrs
        | Seq (e1, e2) ->
            (* Check e1 has type unit, synthesise body type *)
            let (e1_env, e1_constrs) = check_comp ienv decl_env e1 Type.unit_type in
            let (e2_type, e2_env, e2_constrs) = synth e2 in
            let (env, env_constrs) = Ty_env.join ienv e1_env e2_env in
            let constrs =
                Constraint_set.union_many
                    [ e1_constrs; e2_constrs; env_constrs ] in
            (e2_type, env, constrs)
        | Spawn e ->
            let (env, constrs) = check_comp ienv decl_env e Type.unit_type in
            (* Since 'e' will be evaluated in a separate thread, we don't
               care about usage information. (It is of no consequence to the
               spawning thread if the child thread aliases the name, for
               example).
               For the purposes of the spawning thread, we treat all inferred
               usages as Usable. *)
            Type.unit_type, Ty_env.make_usable env, constrs
        | c -> Gripers.cannot_synthesise c

(* Check --> Synth switch. Ensures synthesised type is subtype of checked type. *)
(* Note: This results in the loss of contextual checking information. Really,
   should only be invoked when checking constants, base variables, or
   side-effecting terms. *)
and check_to_synth : IEnv.t -> Ty_env.t -> Ir.comp -> Type.t -> Ty_env.t * Constraint_set.t =
    fun ienv decl_env e check_ty ->
        let synth_ty, synth_env, synth_constrs =
            synthesise_comp ienv decl_env e
        in
        let subty_constrs = subtype ienv synth_ty check_ty in
        synth_env, Constraint_set.union synth_constrs subty_constrs
(* Checking mode *)
and check_comp : IEnv.t -> Ty_env.t -> Ir.comp -> Type.t -> Ty_env.t * Constraint_set.t =
        fun ienv decl_env e ty ->
    let chk = check_comp ienv decl_env in
    let chkv = check_val ienv decl_env in
    match e with
        | Return v -> check_val ienv decl_env v ty
        | Case { term; branch1 = ((bnd1, ty1), comp1); branch2 = ((bnd2, ty2), comp2) } ->
            let (term_env, term_constrs) =
                check_val ienv decl_env term (Type.make_sum_type ty1 ty2)
            in
            let var1 = Var.of_binder bnd1 in
            let var2 = Var.of_binder bnd2 in
            (* Check both branches, and check that inferred types match annotations *)
            let (comp1_env, comp1_constrs) = chk comp1 ty in
            let (comp2_env, comp2_constrs) = chk comp2 ty in
            let env1_constrs = Ty_env.check_type ienv var1 ty1 comp1_env in
            let env2_constrs = Ty_env.check_type ienv var2 ty2 comp2_env in
            (* Calculate merge of the branches (sans binders) *)
            let isect_env, isect_constrs =
                Ty_env.intersect
                    (Ty_env.delete var1 comp1_env)
                    (Ty_env.delete var2 comp2_env)
            in
            (* Finally join the term env with the intersected env *)
            let env, env_constrs =
                Ty_env.join ienv term_env isect_env
            in
            let constrs =
                Constraint_set.union_many
                    [ comp1_constrs; comp2_constrs; env1_constrs;
                      env2_constrs; isect_constrs; env_constrs; term_constrs ]
            in
            (env, constrs)
        | Seq (e1, e2) ->
            (* Check e1 has type unit, check body type *)
            let (e1_env, e1_constrs) = chk e1 Type.unit_type in
            let (e2_env, e2_constrs) = chk e2 ty in
            let (env, env_constrs) = Ty_env.join ienv e1_env e2_env in
            let constrs =
                Constraint_set.union_many
                    [ e1_constrs; e2_constrs; env_constrs ] in
            (env, constrs)

        | If { test; then_expr; else_expr } ->
            let (test_env, test_constrs) = chkv test Type.bool_type in
            let (then_env, then_constrs) = chk then_expr ty in
            let (else_env, else_constrs) = chk else_expr ty in
            let (branches_env, env1_constrs) = Ty_env.intersect then_env else_env in
            (* Doesn't actually matter whether this is 'join' or 'combine',
               since there's no typable value with type 'bool' which will
               involve sending along a mailbox type. Using 'combine' for
               uniformity. *)
            let (env, env2_constrs) = Ty_env.combine ienv test_env branches_env in
            let constrs =
                Constraint_set.union_many
                    [ test_constrs; then_constrs;
                      else_constrs; env1_constrs; env2_constrs ]
            in
            (env, constrs)
        | Let { binder; term; cont = body } ->
            (* Standard BBT let-checking rule. Check body, grab type for binder,
               check binding term has compatible type. Allows us to maintain
               checking context. *)
            let body_env, body_constrs = chk body ty in
            (* Binder might be unused. This is fine if the type is unrestricted. *)
            (* Strategy:
                  - If it's used in the continuation, then check as usual
                  - Otherwise, we need to revert back to synthesis
               This has the potential for incompleteness (for example if we
               have an unrestricted mailbox type)
             *)
            let binder_var = Var.of_binder binder in
            let env, constrs =
                match Ty_env.lookup_opt (Var.of_binder binder) body_env with
                    | Some binder_ty ->
                        (* Check subject of the let, based on the inferred type. *)
                        (* Regardless of whether the inferred type was inferred
                         to be Usable or Returnable, it *must* be treatable as
                         Returnable in the subject of the let. *)
                        let (term_env, term_constrs) =
                            chk term (Type.make_returnable binder_ty)
                        in
                        (* Join environments, union constraints *)
                        let (env, env_constrs) =
                            Ty_env.join ienv term_env body_env
                        in
                        let constrs =
                            Constraint_set.union_many
                                [body_constrs; term_constrs; env_constrs] in
                        (env, constrs)
                    | None ->
                        (* In this case, all we can really do is synthesise and
                           check it's not linear. *)
                        let (binder_ty, binder_env, binder_constrs) =
                            synthesise_comp ienv decl_env term in
                        (* The synthesised type must be returnable *)
                        let () =
                            if not (Type.is_returnable binder_ty) then
                                Gripers.let_not_returnable binder_ty
                        in
                        let () =
                            if Type.is_lin binder_ty then
                                Gripers.unused_synthesised_linear_var
                                binder_var binder_ty
                        in
                        let (env, env_constrs) =
                            Ty_env.join ienv binder_env body_env
                        in
                        let constrs =
                            Constraint_set.union_many
                                [body_constrs; binder_constrs; env_constrs]
                        in
                        (env, constrs)
            in
            (Ty_env.delete binder_var env, constrs)
        (* LetPair is similar to Let. Annoyingly we must revert to synthesis of the pair if
           *either* binder is unused in the continuation. This could maybe be ameliorated somewhat
           if we had wildcard patterns. *)
        | LetPair { binders = (b1, b2); pair; cont = body } ->
            (* Check body type and extract types for binders *)
            let body_env, body_constrs = chk body ty in
            (* Either binder might be unused.
               Revert to synthesis if we don't have type info for both. *)
            let b1var = Var.of_binder b1 in
            let b2var = Var.of_binder b2 in
            let env, constrs =
                match
                    Ty_env.lookup_opt b1var body_env,
                    Ty_env.lookup_opt b2var body_env
                with
                    | Some b1ty, Some b2ty ->
                        (* Note: make_pair_type ensures both types are returnable *)
                        let target_ty =
                            Type.make_pair_type b1ty b2ty
                        in
                        let (term_env, term_constrs) =
                            check_val ienv decl_env pair target_ty
                        in
                        (* Join environments, union constraints *)
                        let (env, env_constrs) =
                            Ty_env.join ienv term_env body_env
                        in
                        let constrs =
                            Constraint_set.union_many
                                [body_constrs; term_constrs; env_constrs] in
                        (env, constrs)
                    | _, _ ->
                        (* In this case, all we can really do is synthesise and
                           check it's not linear. *)
                        let (pair_ty, pair_env, pair_constrs) =
                            synthesise_val ienv decl_env pair
                        in
                        let (pty1, pty2) =
                            match pair_ty with
                                | Type.Pair (pty1, pty2) -> (pty1, pty2)
                                | _ -> Gripers.expected_pair_type pair_ty
                        in
                        (* The synthesised type must be a returnable pair *)
                        let () =
                            if not (Type.is_returnable pair_ty) then
                                Gripers.let_not_returnable pair_ty
                        in
                        let () =
                            if Type.is_lin pty1 then
                                Gripers.unused_synthesised_linear_var
                                b1var pty1
                            else if Type.is_lin pty2 then
                                Gripers.unused_synthesised_linear_var
                                b1var pty2
                        in
                        let (env, env_constrs) =
                            Ty_env.combine ienv pair_env body_env
                        in
                        let constrs =
                            Constraint_set.union_many
                                [body_constrs; pair_constrs; env_constrs]
                        in
                        (env, constrs)
            in
            let env =
                env
                |> Ty_env.delete b1var
                |> Ty_env.delete b2var
            in
            (env, constrs)
        | Guard { iname = None; _ } -> (* Should have been filled in by pre-typing *)
            assert false
        | Guard { target; pattern; guards; iname = Some iname } ->
            let open Type in
            (* Check guard types, and generate constraints, and pattern for guards *)
            let (guards_env, guards_pat, guards_constrs) =
                check_guards ienv decl_env iname pattern guards ty in
            (* Check to see whether the MB handle can be given a type that's
               compatible with the synthesised pattern. *)
            let target_ty = Mailbox {
                capability = In;
                interface = iname;
                pattern = Some guards_pat;
                (* We can only receive on a Returnable guard (since the name
                   goes out of scope afterwards) *)
                quasilinearity = Quasilinearity.Returnable
            } in
            (* Regardless of how it is used in the continuation, the mailbox
               we're receiving from must be returnable. *)
            let (target_env, target_constrs) =
                chkv target (Type.make_returnable target_ty)
            in
            let (env, env_constrs) =
                Nullable_env.combine ienv target_env guards_env
            in
            (* The pattern annotation must be *included* in the inferred pattern.
               The reason this is inclusion rather than equivalence is that we
               should be able to add in additional guards which are not included
               in the annotation (which will necessarily have the unreliable
               mailbox in their continuation and need to discard the MB using
               'fail'.)
             *)
            let pat_constrs =
                Constraint_set.single_constraint pattern guards_pat
            in
            let constrs =
                Constraint_set.union_many
                    [guards_constrs;
                     target_constrs;
                     env_constrs;
                     pat_constrs]
            in
            (env, constrs)
        | e ->
            (* If we don't have a checking rule, it might be possible to
               synthesise and then check the result. *)
            (* This is certainly the case for base variables and constants. *)
            (* We want to avoid this if we can otherwise, though, since it means
               we've lost any contextual information about variable typing. *)
            check_to_synth ienv decl_env e ty

(* Synthesises types for all guards, checks that their types are compatible, and
 * returns the intersection of the resulting environments and generated pattern. *)
and check_guards :
    IEnv.t -> Ty_env.t -> interface_name -> Type.Pattern.t -> Ir.guard list ->
        Type.t -> Nullable_env.t * Type.Pattern.t * Constraint_set.t =
        fun ienv decl_env iname guard_pat gs ty ->
          let open Ir in
          let open Type in
          (* Do a duplication check on guards *)
          let _ =
            List.fold_left (fun (free, fail, tags) x ->
              match x with
                | Free _ ->
                    if free then
                      Gripers.multiple_free ()
                    else
                      (true, fail, tags)
                | Fail ->
                    if fail then
                      Gripers.multiple_fail ()
                    else
                      (free, true, tags)
                | Receive { tag; _ } ->
                    if List.mem tag tags then
                      Gripers.multiple_receive tag
                    else
                      (free, fail, tag :: tags)
            ) (false, false, []) gs in

          (* Typecheck each non-fail guard, and infer an environment. *)
          (* Check types are the same, and calculate the environment merge,
             pattern, and final constraint set. *)
          List.fold_left (fun (env, pat, acc_constrs) g ->
            (* TC Guard *)
            let (g_env, g_pat, g_constrs) =
              check_guard ienv decl_env iname guard_pat g ty
            in
            (* Calculate environment intersection *)
            let (env, env_constrs) = Nullable_env.intersect g_env env in
            let constrs =
              Constraint_set.union_many
                [g_constrs; env_constrs; acc_constrs] in
            (env, Pattern.Plus (pat, g_pat), constrs) )
          (Nullable_env.null, Pattern.Zero, Constraint_set.empty) gs

(* Checks the type for a single guard, returning type, environment, pattern,
   and constraint set. *)
and check_guard :
    IEnv.t -> Ty_env.t -> interface_name -> Type.Pattern.t -> Ir.guard -> Type.t ->
        Nullable_env.t * Type.Pattern.t * Constraint_set.t =
        fun ienv decl_env iname pat g ty ->
          let open Ir in
          let open Type in
          match g with
            | Receive { tag; payload_binders; mailbox_binder; cont } ->
                let (env, cont_constrs) = check_comp ienv decl_env cont ty in
                let payload_iface_tys =
                        IEnv.lookup iname ienv
                        |> Interface.lookup tag
                in
                (* Check that the received values have types consistent with the
                   interface annotations. *)
                let check_payload_ty payload_binder interface_ty =
                    match Ty_env.lookup_opt (Var.of_binder payload_binder) env with
                        | Some payload_ty ->
                            (* The type we're checking against has to only be usable:
                               we can't have the received value escaping the
                               receive block, otherwise we get the potential for
                               aliasing. *)
                            subtype ienv
                                (Type.make_usable interface_ty)
                                payload_ty
                        | None ->
                            Type_utils.make_unrestricted interface_ty
                in
                let payload_ty_constrs =
                    List.combine payload_binders payload_iface_tys
                    |> List.fold_left (fun constrs (bnd, iface_ty) ->
                        check_payload_ty bnd iface_ty
                        |> Constraint_set.union constrs
                    ) Constraint_set.empty
                in
                let mb_ty =
                    Ty_env.lookup_opt (Var.of_binder mailbox_binder) env in
                (* Grab types for payload and MB binders *)
                let mb_pat =
                  match mb_ty with
                    | Some Mailbox {
                        capability = Capability.In; pattern = Some pat; _ } -> pat
                    | Some ty ->
                        Gripers.expected_receive_mailbox
                            (Var.of_binder mailbox_binder)
                            ty
                    | None ->
                        Gripers.unused_mailbox_variable
                            (Var.of_binder mailbox_binder)
                in

                (* Delete payload and MB binders from calculated env *)
                let env =
                  List.fold_right (Ty_env.delete_binder) payload_binders env
                  |> Ty_env.delete_binder mailbox_binder
                in
                (* If we are receiving a mailbox variable, without a dependency
                   graph, we must do some fairly coarse-grained aliasing
                   control.
                   There are three strategies:
                      1. Strict: the environment must be entirely unrestricted
                          (i.e., no other mailbox variables are free in the receive
                          block)

                      2. Interface: the environment cannot contain a variable of
                          the same interface. This means that we know we won't
                          accidentally alias, without being overly restrictive.

                      3. Nothing: No alias control: permissive, but unsafe.
                  *)
                let () =
                    let mb_iface_tys =
                        List.filter_map (fun ty ->
                            if Type.is_mailbox_type ty then
                                Some (Type.get_interface ty)
                            else None)
                        payload_iface_tys
                    in
                    let open Settings in
                    let open ReceiveTypingStrategy in
                    match get receive_typing_strategy with
                        | Strict ->
                            Ty_env.iter (fun v ty ->
                                if Type.is_lin ty then
                                    Gripers.unrestricted_recv_env v ty
                            ) env
                        | Interface ->
                            Ty_env.iter (fun v ty ->
                                match ty with
                                    | Mailbox { interface; _ } when (List.mem interface mb_iface_tys) ->
                                        Gripers.duplicate_interface_receive_env
                                        v interface
                                    | _ -> ()
                            ) env
                        | Nothing -> ()
                in
                (* Calculate the derivative wrt. the tag, and ensure via a
                   constraint that it is included in the calculated payload
                   type. *)
                let deriv = Pattern.tag_derivative tag pat in
                let constrs =
                      Constraint_set.single_constraint deriv mb_pat
                      |> Constraint_set.union cont_constrs
                      |> Constraint_set.union payload_ty_constrs
                in
                (* Final calculated pattern is the message concatenated with the
                   calculated resulting pattern. *)
                let res_pat =  Pattern.(Concat (Message tag, deriv)) in
                (Nullable_env.of_env env, res_pat, constrs)
            | Free e ->
                let (env, constrs) = check_comp ienv decl_env e ty in
                (Nullable_env.of_env env, One, constrs)
            | Fail ->
                (Nullable_env.null, Zero, Constraint_set.empty)

(* Declarations have a top-level annotation, so it only makes sense to check them. *)
(* The result of checking each definition should be a typing environment with a
   reference to the definition, and calls to any other definition. We should
   check that each variable contained in the output is unbound.

   NOTE: Although it may be the case that a call to a function is inferred to
   have subtypes of arguments & supertype of result, we must retain the original
   type for each declaration.
*)
let check_decls ienv decls =
    (* List of allowed free names: all declaration names. Primitive names won't
     get added into the environment at all. *)
    let allowed_free_names =
        List.map (fun d -> Var.of_binder d.decl_name) decls
    in
    let decl_env =
        let decl_entry d =
            let args = List.map snd d.decl_parameters in
            Var.of_binder d.decl_name,
            Type.make_function_type false args d.decl_return_type
        in
        List.map decl_entry decls
        |> Ty_env.from_list
    in

    (* Function which checks inferred environment against reference environment,
       returning subtyping constraints *)
    (* Note: we don't need to do any checks to ensure that the variables have
       been used in a way consistent with the annotations on each declaration;
       this will have been done in the App case. *)
    let check_free_vars : Ir.Var.t -> Ty_env.t -> unit =
        fun decl_name env ->
            Ty_env.iter (fun x _ ->
                if not (List.mem x allowed_free_names) then
                    Gripers.unexpected_free_var x decl_name) env
    in

    (* Function which typechecks the body of a declaration, ensuring usages of
       arguments are consistent with their annotations.
       Returns the inferred typing environment for the declaration (with
       arguments removed), along with generated constraints. *)
    let check_decl d =
        let (env, body_constrs) =
            check_comp ienv decl_env d.decl_body d.decl_return_type in
        (* Check arguments; remove from environment *)
        let (env, arg_constrs) =
            (* Each inferred argument type should be a subtype of the
               declared type. *)
            List.fold_left (fun (acc_env, acc_constrs) (param, param_ty) ->
                let param_constrs =
                    (* It may be the case that the parameter is actually unused
                       in the function body. This is fine, as long as the type
                       is unrestricted.
                     *)
                    match Ty_env.lookup_opt (Var.of_binder param) acc_env with
                        | Some inferred_param_ty ->
                            subtype ienv param_ty inferred_param_ty
                        | None ->
                            make_unrestricted param_ty
                in
                (Ty_env.delete_binder param acc_env,
                 Constraint_set.union param_constrs acc_constrs)
            ) (env, Constraint_set.empty) d.decl_parameters
        in
        (* Check environment is consistent with reference environment *)
        check_free_vars (Var.of_binder d.decl_name) env;
        Constraint_set.union
            body_constrs
            arg_constrs
    in

    let decl_constrs =
        List.map check_decl decls
        |> Constraint_set.union_many
    in
    (* Finally, return reference environment and gathered constraints *)
    decl_env, decl_constrs


(* PRECONDITION: By now, annotation pass should have run, so each of the
   interfaces should be decorated with at least a pattern variable. *)
let synthesise_program { prog_interfaces; prog_decls; prog_body } =
    let ienv = IEnv.from_list prog_interfaces in
    let (decl_env, decl_constrs) = check_decls ienv prog_decls in
    (* If we have a body, synthesise type and combine environments.
       Otherwise, return unit and the decl env / constraints. *)
    match prog_body with
        | Some body ->
            let ty, body_env, body_constrs = synthesise_comp ienv decl_env body in
            let env, env_constrs = Ty_env.combine ienv decl_env body_env in
            let constrs =
                Constraint_set.union_many
                    [decl_constrs; body_constrs; env_constrs]
            in
            ty, env, constrs
        | None ->
            Type.unit_type, decl_env, decl_constrs

(* Similar to the above, but checks the body type rather than
   synthesising it. *)
let check_program { prog_interfaces; prog_decls; prog_body } ty =
    let ienv = IEnv.from_list prog_interfaces in
    let (decl_env, decl_constrs) = check_decls ienv prog_decls in
    (* If we have a body, synthesise type and combine environments.
       Otherwise,  *)
    match prog_body with
        | Some body ->
            let body_env, body_constrs = check_comp ienv decl_env body ty in
            let env, env_constrs = Ty_env.combine ienv decl_env body_env in
            let constrs =
                Constraint_set.union_many
                    [decl_constrs; body_constrs; env_constrs]
            in
            env, constrs
        | None ->
            decl_env, decl_constrs
