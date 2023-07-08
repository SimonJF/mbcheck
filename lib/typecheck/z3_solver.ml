(* Translates a Presburger formula into a Z3 expression, and checks its satisfiability. *)
open Z3
open Util.Utility

(* Timeout used for Z3: 10 seconds *)
let z3_timeout = 10000

(* The Z3 quantifier API is so horrible that we need some helpers... *)
(* Translates an existential quantifier, adding defaults for the unneeded arguments. *)
let mk_quantifier_simple mk_fn ctx sorts symbs body =
    mk_fn ctx sorts symbs body (Some 1) [] [] None None
    |> Quantifier.expr_of_quantifier

(* Translates an existential quantifier for the simple case that we have a single
   integer variable.*)
let mk_quantifier_int mk_fn ctx symb body =
    mk_quantifier_simple
        mk_fn
        ctx
        [Arithmetic.Integer.mk_sort ctx]
        [Symbol.mk_string ctx symb]
        body

let mk_exists_int = mk_quantifier_int Quantifier.mk_exists

let mk_forall_int = mk_quantifier_int Quantifier.mk_forall

(* Note that this is explicitly ANF'ed. I don't think it's entirely necessary,
   but since the operations are side-effecting, it's potentially worth it. *)
let z3_of_presburger ctx =
    let open Presburger in
    let int_sort () = Arithmetic.Integer.mk_sort ctx in
    let mk_int = Arithmetic.Integer.mk_numeral_i ctx in

    (* Translation requires an explicit translation environment for bound names,
       and we also need to shift all existing variables under every forall binder. *)
    let rec translate_expr env =
        function
            | Var v ->
                let sort = int_sort () in
                Quantifier.mk_bound ctx (StringMap.find v env) sort
            | Int i -> mk_int i
            | Add (e1, e2) ->
                let e1 = translate_expr env e1 in
                let e2 = translate_expr env e2 in
                Arithmetic.mk_add ctx [e1; e2]
            | Mul (n, e) ->
                let e = translate_expr env e in
                Arithmetic.mk_mul ctx [mk_int n; e]
    in
    let rel_fn = function
        | EQ -> Boolean.mk_eq
        | LE -> Arithmetic.mk_le
        | LT -> Arithmetic.mk_lt
    in
    let shift_env env = StringMap.map (fun x -> x + 1) env in
    let rec translate env = function
        | True -> Boolean.mk_true ctx
        | False -> Boolean.mk_false ctx
        | Rel (rel, x1, x2) ->
            let x1 = translate_expr env x1 in
            let x2 = translate_expr env x2 in
            (rel_fn rel) ctx x1 x2
        | And (x1, x2) ->
            let x1 = translate env x1 in
            let x2 = translate env x2 in
            Boolean.mk_and ctx [x1; x2]
        | Or (x1, x2) ->
            let x1 = translate env x1 in
            let x2 = translate env x2 in
            Boolean.mk_or ctx [x1; x2]
        | Not x ->
            let x = translate env x in
            Boolean.mk_not ctx x
        | Forall (v, x) ->
            let env = StringMap.add v 0 (shift_env env) in
            let x = translate env x in
            mk_forall_int ctx v x
        | Exists (v, x) ->
            let env = StringMap.add v 0 (shift_env env) in
            let x = translate env x in
            mk_exists_int ctx v x
    in
    translate (StringMap.empty)

let result_of_z3_result : Solver.status -> Solver_result.t =
    let open Solver_result in
    function
        | SATISFIABLE -> Satisfiable
        | UNSATISFIABLE -> Unsatisfiable
        | UNKNOWN -> Unknown

let solve : Presburger.goal -> Solver_result.t = fun { tags; lhs; rhs } ->
    let ctx = mk_context
        [("model", "false"); ("proof", "false"); ("timeout", string_of_int z3_timeout)]
    in
    let expr =
        let open Presburger in
        List.fold_right
            (fun x acc -> Forall (x, acc))
            tags
            (* Translate equivalent formulation of implication, so that the
               environment is correctly set *)
            (Or (Not lhs, rhs))
    in
    let z3_expr = z3_of_presburger ctx expr in
    Common.Settings.if_debug (fun () ->
        Printf.printf "DEBUG -- Z3 PRESBURGER: %s\n" (Expr.to_string z3_expr)
    );
    let params = Z3.Params.mk_params ctx in
    Z3.Params.add_int params (Z3.Symbol.mk_string ctx "timeout") 500;
    (* For some reason, we need to explicitly tell OCaml's Z3 which tactics
       to use (in our case, quantifier elimination followed by the quantifier-free
       linear integer arithmetic solver.

       I don't know why this is, and it took me 2 days to figure this out, but
       there you go.
    *)
    let tac1 = Tactic.mk_tactic ctx "qe" in
    let tac2 = Tactic.mk_tactic ctx "qflia" in
    let tac = Tactic.par_and_then ctx tac1 tac2 in
    let solver = Solver.mk_solver_t ctx tac in
    Z3.Solver.set_parameters solver params;
    Solver.check solver [z3_expr]
    |> result_of_z3_result

