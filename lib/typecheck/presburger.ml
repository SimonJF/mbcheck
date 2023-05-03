(* Presburger Formulae *)
type var = string

type relation = EQ | LE | LT

type expr =
    | Var of var
    | Int of int
    | Add of (expr * expr)
    | Mul of (int * expr)

type t =
    | False
    | True
    | Rel of (relation * expr * expr)
    | And of (t * t)
    | Or of (t * t)
    | Not of t
    | Forall of (var * t)
    | Exists of (var * t)


(* Goal: Want to check whether lhs implies rhs, given the tags *)
type goal = { tags: string list; lhs: t; rhs: t }

let make_goal tags lhs rhs = { tags; lhs; rhs }

(* Optimising constructors for And and Or *)
let conj p1 p2 =
    match p1, p2 with
        | _, False | False, _ -> False
        | p, True  | True,  p -> p
        | p1, p2 -> And (p1, p2)

let disj p1 p2 =
    match p1, p2 with
        | p, False | False, p -> p
        | _, True  | True, _  -> True
        | p1, p2 -> Or (p1, p2)

let tag_eq_lit tag n = Rel (EQ, Var tag, Int n)
let tag_eq tag expr = Rel (EQ, Var tag, expr)
let tt = True
let ff = False
let plus e1 e2 =
    match e1, e2 with
        | e, Int 0 | Int 0, e -> e
        | _, _ -> Add (e1, e2)


let rec pp_expr ppf =
    let open Format in
    function
        | Var var -> pp_print_string ppf var
        | Int i -> pp_print_int ppf i
        | Add (e1, e2) ->
            Format.fprintf ppf "%a + %a" pp_expr e1 pp_expr e2
        | Mul (n, e) ->
            Format.fprintf ppf "%d * %a" n pp_expr e

let pp_rel ppf =
    let open Format in
    function
        | EQ -> pp_print_string ppf "="
        | LE -> pp_print_string ppf "≤"
        | LT -> pp_print_string ppf "<"

let rec pp ppf =
    let open Format in
    function
        | False -> pp_print_string ppf "⊥"
        | True -> pp_print_string ppf "⊤"
        | Rel (rel, e1, e2) ->
            fprintf ppf "%a %a %a" pp_expr e1 pp_rel rel pp_expr e2
        | And (x1, x2) ->
            fprintf ppf "(%a ∧ %a)" pp x1 pp x2
        | Or (x1, x2) ->
            fprintf ppf "(%a ∨ %a)" pp x1 pp x2
        | Not x -> fprintf ppf "¬%a" pp x
        | Forall (var, x) -> fprintf ppf "∀%s. %a" var pp x
        | Exists (var, x) -> fprintf ppf "∃%s. %a" var pp x


let pp_goal ppf { tags; lhs; rhs } =
    let open Format in
    fprintf ppf "[%a] %a ⟶ %a"
        (Util.Utility.pp_print_comma_list pp_print_string) tags
        pp lhs
        pp rhs
