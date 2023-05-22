(* Various operations on types that only arise during typechecking. *)
open Common
open Common_types


(* Tries to ensure that a type is treated as unrestricted. All base types are
   unrestricted. Output mailbox types cannot be made unrestricted. Input mailbox
   types are unrestricted if they are equivalent to 1.
 *)
let make_unrestricted t =
    let open Type in
    match t with
        (* Trivially unrestricted *)
        | Base _
        | Fun { linear = false; _ } -> Constraint_set.empty
        (* Must be unrestricted *)
        | Fun { linear = true; _ }
        | Mailbox { capability = Capability.In; _ } ->
            Gripers.cannot_make_unrestricted t
        (* Generate a pattern constraint in order to ensure linearity *)
        | Mailbox { capability = Capability.Out; pattern = Some pat; _ } ->
                Constraint_set.of_list
                    [Constraint.make (Pattern.One) pat]
        | _ -> assert false

(* Auxiliary definitions*)

(* Checks whether t1 is a subtype of t2, and produces the necessary constraints.
   We need to take a coinductive view of subtyping to avoid infinite loops, so
   we track the visited interface names.
 *)
let rec subtype_type :
    (interface_name * interface_name) list ->
        Interface_env.t -> Type.t -> Type.t -> Constraint_set.t =
    fun visited ienv t1 t2 ->
        match t1, t2 with
            | Base b1, Base b2 when b1 = b2->
                        Constraint_set.empty

            (* Subtyping covariant for pairs and sums *)
            | Pair (tya1, tya2), Pair (tyb1, tyb2)
            | Sum (tya1, tya2), Sum (tyb1, tyb2) ->
                Constraint_set.union
                    (subtype_type visited ienv tya1 tyb1)
                    (subtype_type visited ienv tya2 tyb2)
            | Mailbox { pattern = None; _ }, _
            | _, Mailbox { pattern = None; _ } ->
                    (* Should have been sorted by annotation pass *)
                    assert false
            | Fun { linear = lin1; args = args1;
                    result = body1 },
              Fun { linear = lin2; args = args2;
                    result = body2 } ->
                    let () =
                        if lin1 <> lin2 then
                            Gripers.subtype_linearity_mismatch t1 t2
                    in
                    (* Args contravariant; body covariant *)
                    let args_constrs =
                        List.map2 (subtype_type visited ienv) args2 args1
                        |> Constraint_set.union_many in
                    let body_constrs = subtype_type visited ienv body1 body2 in
                    Constraint_set.union args_constrs body_constrs
            | Mailbox {
                capability = capability1;
                interface = iname1;
                pattern = Some pat1;
                quasilinearity = ql1
              },
              Mailbox {
                capability = capability2;
                interface = iname2;
                pattern = Some pat2;
                quasilinearity = ql2
              } ->
                  (* First, ensure interface subtyping *)
                  let interface1 = Interface_env.lookup iname1 ienv in
                  let interface2 = Interface_env.lookup iname2 ienv in
                  let () =
                      if not (Type.Quasilinearity.is_sub ql1 ql2) then
                          Gripers.quasilinearity_mismatch t1 t2
                  in
                  let iface_constraints =
                      subtype_interface visited ienv interface1 interface2 in
                  let pat_constraints =
                      if capability1 = capability2 then
                          match capability1 with
                            | In ->
                                (* Input types are covariant *)
                                Constraint_set.single_constraint pat1 pat2
                            | Out ->
                                (* Output types are contravariant *)
                                Constraint_set.single_constraint pat2 pat1
                      else
                          Gripers.subtype_cap_mismatch t1 t2
                  in
                  Constraint_set.union iface_constraints pat_constraints
            | _, _ ->
                Gripers.subtype_mismatch t1 t2

and subtype_interface :
    (interface_name * interface_name) list ->
        Interface_env.t -> Interface.t -> Interface.t -> Constraint_set.t =
        fun visited ienv i1 i2 ->
            if List.mem (Interface.name i1, Interface.name i2) visited then
                Constraint_set.empty
            else
                (* Interface i1 is a subtype of interface i2 if i2 supports all
                 messages that i1 supports, and the payloads of i1's messages
                 are subtypes of those of i2. *)
                let visited = (Interface.name i1, Interface.name i2) :: visited in
                List.fold_left (fun acc (tag, payloads1) ->
                    let payloads2 = Interface.lookup tag i2 in
                    List.combine payloads1 payloads2
                    |> List.map (fun (p1, p2) -> subtype_type visited ienv p1 p2)
                    |> Constraint_set.union_many
                    |> Constraint_set.union acc
                ) Constraint_set.empty (Interface.bindings i1)

(** subtype ienv t1 t2 checks whether t1 is a subtype of t2, and generates the
    relevant set of constraints. Wraps around subtype_type. *)
let subtype = subtype_type []
