(* Set of constraints, allowing n-ary unions *)
include Set.Make(Constraint)
open Common.Type
open Util.Utility

let union_many = List.fold_left (union) empty

let single_constraint p1 p2 =
    if p1 = p2 then empty else
        Constraint.make p1 p2 |> singleton

let equivalence_constraint p1 p2 =
    if p1 = p2 then empty else
        of_list [Constraint.make p1 p2; Constraint.make p2 p1]

let pp ppf x =
    Format.fprintf ppf "%a"
        (pp_print_newline_list Constraint.pp)
        (elements x)

(* Simplifies all patterns in constraints, then removes duplicates *)
let simplify =
    filter_map (fun c ->
        let (p1, p2) =
            Constraint.((lhs c |> Pattern.simplify,
                         rhs c |> Pattern.simplify))
        in
        if p1 = p2 then None else Some (Constraint.make p1 p2))

(* Returns a set of pattern variables contained within the constraint set. *)
let pattern_variables cset =
    fold (fun x acc ->
        let pvs1 = Pattern.variables (Constraint.lhs x) in
        let pvs2 = Pattern.variables (Constraint.rhs x) in
        StringSet.union acc (StringSet.union pvs1 pvs2)
    ) cset StringSet.empty
