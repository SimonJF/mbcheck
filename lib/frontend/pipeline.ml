open Common


let desugar p =
    p
    |> Desugar_sugared_guards.desugar
    |> Desugar_let_annotations.desugar
    |> Insert_pattern_variables.annotate


(* Frontend pipeline *)
let pipeline p =
    let p = desugar p in
    let ir = Sugar_to_ir.transform p in
    (*
    let () = Format.printf
        "=== Intermediate Representation: ===\n%a\n\n"
        (Common.Ir.pp_program) ir
    in
    *)
    let ir, prety_opt = Typecheck.Pretypecheck.check ir in
    let (ty, env, constrs) = Typecheck.Gen_constraints.synthesise_program ir in
    let solution = Typecheck.Solve_constraints.solve_constraints constrs in
    let p = Sugar_ast.substitute_solution solution p in
    let ir = Ir.substitute_solution solution ir in
    (p, prety_opt, ir, ty, env, constrs)
