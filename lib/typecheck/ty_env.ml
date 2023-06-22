open Common
open Util.Utility
open Type_utils

module type VARMAP = (Map.S with type key = Ir.Var.t)
module VarMap = Map.Make(Ir.Var)

type t = Type.t VarMap.t

let empty = VarMap.empty

let bind = VarMap.add
let lookup = VarMap.find

let lookup_opt = VarMap.find_opt

let delete = VarMap.remove
let delete_binder x = VarMap.remove (Ir.Var.of_binder x)
let singleton = VarMap.singleton
let bindings = VarMap.bindings
let iter = VarMap.iter

let union = VarMap.union

let from_list xs = List.to_seq xs |> VarMap.of_seq


(* Combines two environments. The environments should only intersect
   on unrestricted types. *)
let combine : Interface_env.t -> t -> t -> t * Constraint_set.t =
    fun ienv env1 env2 ->
        (* Types must be *the same* and *unrestricted* *)
        (* Subtype in both directions *)
        let join_types var (ty1: Type.t) (ty2: Type.t) =
            (* The subtyping and constraints are enough to rule out
               re-use of mailboxes, but it's worth special-casing to
               get a better error message. *)
            if Type.contains_mailbox_type ty1 && Type.contains_mailbox_type ty2
            then
                Gripers.combine_mailbox_type var
            else
                Constraint_set.union_many
                    [
                        make_unrestricted ty1;
                        make_unrestricted ty2;
                        subtype ienv ty1 ty2;
                        subtype ienv ty2 ty1
                    ]
        in
        (* Find the overlapping keys, zip up, and join. *)
        (* Since the subtyping in both directions will ensure equality of types,
           and that the relevant constraints are generated, it is safe to just
           use either type in the combined environment. *)
        let overlap_constrs =
            bindings env1
            |> List.filter_map (fun (k, ty1) ->
                    match lookup_opt k env2 with
                        | None -> None
                        | Some ty2 -> Some (join_types k ty1 ty2)

            )
            |> Constraint_set.union_many
        in
        let combined_env =
            union (fun _ ty1 _ -> Some ty1) env1 env2
        in
        (combined_env, overlap_constrs)

(* Joins two sequential or concurrent environments (i.e., where *both*
   actions will happen). *)
let join : Interface_env.t -> t -> t -> t * Constraint_set.t =
    fun ienv env1 env2 ->

        let join_mailbox_types var mb1 mb2 =
            let open Type in
            let open Capability in
            match mb1, mb2 with
                | (In, _), (In, _) ->
                    Gripers.join_two_recvs var
                (* 2 output capabilities: resulting pattern is a conjunction *)
                | (Out, pat1), (Out, pat2) ->
                        (Out, Pattern.Concat (pat1, pat2)), Constraint_set.empty
                (* Out / in elimination. Generate new pattern pat, and ensure
                   a constraint that pat1 . pat is included in pat2. *)
                | (Out, out_pat), (In, in_pat)
                | (In, in_pat), (Out, out_pat) ->
                        let pat = Pattern.fresh () in
                        let constrs = Constraint_set.single_constraint
                            (Concat (out_pat, pat)) in_pat
                        in
                        ((In, pat), constrs)
        in

        let join_types (var: Ir.Var.t) (t1: Type.t) (t2: Type.t) : (Type.t * Constraint_set.t) =
            let open Type in
            match (t1, t2) with
                | Base b1, Base b2 when b1 = b2 ->
                    (Base b1, Constraint_set.empty)
                | Fun { linear = linear1; args = dom1; result = cod1 },
                  Fun { linear = linear2; args = dom2; result = cod2 }
                    when (not linear1) && (not linear2) && dom1 = dom2 && cod1 = cod2 ->
                        let subty_constrs =
                            Constraint_set.union
                                (subtype ienv t1 t2)
                                (subtype ienv t2 t1)
                        in
                        (Fun { linear = false; args = dom1; result = cod1 }, subty_constrs)
                | Mailbox { pattern = None; _ }, _ | _, Mailbox { pattern = None; _ } ->
                    assert false (* Set by pre-typing *)
                | Mailbox { capability = cap1; interface = iface1; pattern =
                    Some pat1; quasilinearity = ql1 },
                  Mailbox { capability = cap2; interface = iface2; pattern =
                      Some pat2; quasilinearity = ql2 } ->
                      (* We can only join variables with the same interface
                         name. If these match, we can join the types. *)
                      if iface1 <> iface2 then
                          Gripers.env_interface_mismatch true
                            t1 t2 var iface1 iface2
                      else
                          (* Check sequencing of QL *)
                          let ql =
                              match Quasilinearity.sequence ql1 ql2 with
                                | Some ql -> ql
                                | None ->
                                    Gripers.invalid_ql_sequencing var
                          in
                          let ((cap, pat), constrs) =
                              join_mailbox_types var
                                (cap1, pat1) (cap2, pat2) in
                          Mailbox {
                              capability = cap;
                              interface = iface1;
                              pattern = Some pat;
                              quasilinearity = ql
                          }, constrs
                | _, _ ->
                    Gripers.type_mismatch true t1 t2 var
        in
        (* Calculate intersection of the two typing environments, and join
           each type. *)
        let bindings1 = VarMap.bindings env1 in
        let bindings2 = VarMap.bindings env2 in

        let (isect1, disjoint1) =
            List.partition (fst >> (flip List.mem_assoc) bindings2) bindings1 in
        let (isect2, disjoint2) =
            List.partition (fst >> (flip List.mem_assoc) bindings1) bindings2 in

        (* Now, combine intersecting lists *)
        let (joined, constrs) =
            List.fold_left (fun (joined, constrs) (name, ty1) ->
                let ty2 = List.assoc name isect2 in
                let (ty, join_constrs) = join_types name ty1 ty2 in
                ((name, ty) :: joined, Constraint_set.union join_constrs constrs)
            ) ([], Constraint_set.empty) isect1 in
        from_list (joined @ disjoint1 @ disjoint2), constrs

(* Merges environments resulting from branching control flow. *)
(* Core idea is that linear types must be used in precisely the same way in
    each branch. Unrestricted types must be used at the same type, but need
    not be present in each branch (and will appear in the output environment.) *)
let intersect : t -> t -> t * Constraint_set.t =
    fun env1 env2 ->
        let open Type in
        let intersect_mailbox_types var mb1 mb2 =
            let open Capability in
            match mb1, mb2 with
                | (Out, pat1), (Out, pat2) ->
                    (* Disjunctive -- either send this, or send that *)
                    (Out, Pattern.Plus (pat1, pat2)), Constraint_set.empty
                | (In, pat1), (In, pat2) ->
                    (* Behaviour must be supported by *both* branches *)
                    (* Check this by generating a new pattern, and ensuring
                       that the new pattern is included in both patterns. *)
                    let pat = Pattern.fresh () in
                    let constrs =
                      [ Constraint.make pat pat1; Constraint.make pat pat2 ]
                      |> Constraint_set.of_list in
                    (In, pat), constrs
                | _, _ ->
                    Gripers.inconsistent_branch_capabilities var
        in
        let intersect_types var (t1: Type.t) (t2: Type.t) : (Type.t * Constraint_set.t) =
            match t1, t2 with
                | Base b1, Base b2 when b1 = b2 ->
                    (Base b1, Constraint_set.empty)
                | Fun { linear = linear1; args = dom1; result = cod1 },
                  Fun { linear = linear2; args = dom2; result = cod2 }
                    when (linear1 = linear2) && dom1 = dom2 && cod1 = cod2 ->
                        (Fun { linear = linear1; args = dom1; result = cod1 }, Constraint_set.empty)
                | Mailbox { pattern = None; _ }, _ | _, Mailbox { pattern = None; _ } ->
                    assert false (* Set by pre-typing *)
                | Mailbox { capability = cap1; interface = iface1; pattern =
                    Some pat1; quasilinearity = ql1 },
                  Mailbox { capability = cap2; interface = iface2; pattern =
                      Some pat2; quasilinearity = ql2 } ->
                      (* As before -- interface names must be the same*)
                      if iface1 <> iface2 then
                          Gripers.env_interface_mismatch
                            false t1 t2 var iface1 iface2
                      else
                          let ((cap, pat), constrs) =
                              intersect_mailbox_types var
                                (cap1, pat1) (cap2, pat2) in
                          Mailbox {
                              capability = cap;
                              interface = iface1;
                              pattern = Some pat;
                              (* Must take strongest QL across all branches. *)
                              quasilinearity = Quasilinearity.max ql1 ql2
                          }, constrs
                | _, _ ->
                    Gripers.type_mismatch false t1 t2 var
        in

        (* As in the join case, calculate intersection of the two typing
           environments, and join each type. *)
        let intersect_envs (env1: Type.t VarMap.t) (env2: Type.t VarMap.t) =
            let bindings1 = VarMap.bindings env1 in
            let bindings2 = VarMap.bindings env2 in

            let (isect1, disjoint1) =
                List.partition (fst >> (flip List.mem_assoc) bindings2) bindings1 in
            let (isect2, disjoint2) =
                List.partition (fst >> (flip List.mem_assoc) bindings1) bindings2 in

            (* Perform safety checks on types appearing in one branch but not the other *)
            (* For mailbox types: receive mailboxes *must* be used in both branches.
               Send mailbox types may be used in only one -- but then we infer an unrestricted
               binding !1 for the other branch.

               For other types: can be used in one branch only, since they are unrestricted.
             *)
            let disjoints =
              let (disjoint_sends, disjoint_others) =
                List.partition (fun (_, ty) ->
                  Type.is_output_mailbox ty
                ) (disjoint1 @ disjoint2)
              in
              (* !E becomes !(E + 1) *)
              let disjoint_sends =
                List.map (fun (var, ty) ->
                  let ty =
                    match ty with
                      | Mailbox { capability = Out; interface; quasilinearity; pattern = Some pat } ->
                          let pat = Pattern.Plus (pat, Pattern.One) in
                          Mailbox { capability = Out; interface; quasilinearity; pattern = Some pat }
                      | _ ->
                          raise (Errors.internal_error "ty_env.ml" "error in disjoint MB combination")
                  in
                  (var, ty)
                ) disjoint_sends
              in

              let () =
                List.iter (fun (var, ty) ->
                    if Type.is_lin ty then
                        Gripers.branch_linearity var
                    else ()
                ) disjoint_others
              in
              disjoint_sends @ disjoint_others
            in
            (* Then, calculate intersection of the types appearing in both
               environments *)
            let (merged, constrs) =
                List.fold_left (fun (acc, constrs) (name, ty1) ->
                    let ty2 = List.assoc name isect2 in
                    let (ty, isect_constrs) = intersect_types name ty1 ty2 in
                    ((name, ty) :: acc, Constraint_set.union isect_constrs constrs)
                ) ([], Constraint_set.empty) isect1
            in
            from_list (merged @ disjoints), constrs
        in
        intersect_envs env1 env2

let dump env =
    VarMap.bindings env
    |> List.iter (fun (x, ty) ->
            Format.(fprintf std_formatter "%s : %a\n%!"
                (Ir.Var.unique_name x)
                Type.pp ty))

let make_usable =
    VarMap.map Type.make_usable

let make_returnable =
    VarMap.map Type.make_returnable

let check_type ienv var declared env =
    match lookup_opt var env with
        | Some inferred -> subtype ienv declared inferred
        | None -> Type_utils.make_unrestricted declared

let make_unrestricted env =
    List.fold_left (fun acc (_, ty) ->
        Constraint_set.union acc (make_unrestricted ty)
    ) Constraint_set.empty (bindings env)

