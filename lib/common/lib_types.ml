(* Type signatures for primitive functions.
 * Just a stub for now; will fill it in later.
 *)
open Common_types

let signatures =
    let open Type in
    let open Base in
    let int_op_type = function_type false [Base Int; Base Int] (Base Int) in
    let int_rel_op_type = function_type false [Base Int; Base Int] (Base Bool) in
    let bool_rel_op_type = function_type false [Base Bool; Base Bool] (Base Bool) in
    let int_ops =
        List.map (fun x -> (x, int_op_type)) ["+"; "-"; "/"; "*"]
    in
    let int_rel_ops =
        List.map (fun x -> (x, int_rel_op_type)) ["<"; "<="; ">"; ">="; "!="; "=="]
    in
    let bool_rel_ops =
        List.map (fun x -> (x, bool_rel_op_type)) ["&&"; "||"]
    in
    int_ops @ int_rel_ops @ bool_rel_ops @
    [
        ("print", function_type false [Base Base.String] (Base Base.Unit));
        ("concat", function_type false [Base Base.String; Base Base.String] (Base Base.String));
        ("rand", function_type false [Base Base.Int] (Base Base.Int));
        ("sleep", function_type false [Base Base.Int] (Base Base.Unit));
        ("intToString", function_type false [Base Base.Int] (Base Base.String))
    ]
