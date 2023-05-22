(* Operations on nullable type environments.
   A small wrapper over type environments.
   We only need to support intersection between two nullable environments,
   and disjoint combination with a defined environment.
 *)
type t = Ty_env.t option

let intersect nenv1 nenv2 =
    match nenv1, nenv2 with
        | None, None -> None, Constraint_set.empty
        | Some env, None | None, Some env -> Some env, Constraint_set.empty
        | Some env1, Some env2 ->
            let env, constrs = Ty_env.intersect env1 env2 in
            Some env, constrs

let combine ienv env1 nenv =
    match nenv with
        | Some env2 -> Ty_env.combine ienv env1 env2
        | _ -> env1, Constraint_set.empty

let of_env env = Some env
let null = None
