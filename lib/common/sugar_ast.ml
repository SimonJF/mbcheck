open Common_types
open Util.Utility
open Format

(* Basic sugared AST *)
(* Expressions *)
type expr =
    | Var of sugar_var
    | Primitive of (primitive_name[@name "primitive_name"])
    (* Type annotation, used for synthesis --> checking switch. *)
    | Annotate of expr * (Type.t[@name "ty"])
    | Constant of constant
    | Lam of {
        linear: bool;
        parameters: (sugar_binder * (Type.t[@name "ty"])) list;
        result_type: (Type.t[@name "ty"]);
        body: expr }
    | Let of {
        binder: sugar_binder;
        annot: (Type.t[@name "ty"]) option;
        term: expr;
        body: expr }
    | Seq of expr * expr
    | App of {
        func: expr;
        args: expr list
    }
    | If of { test: expr; then_expr: expr; else_expr: expr }
    (* Pairs *)
    | Pair of (expr * expr)
    | LetPair of {
        binders: sugar_binder * sugar_binder;
        term: expr;
        cont: expr
    }
    (* Sums *)
    | Inl of expr
    | Inr of expr
    | Case of {
        term: expr;
        branch1: (sugar_binder * (Type.t[@name "ty"])) * expr;
        branch2: (sugar_binder * (Type.t[@name "ty"])) * expr
    }
    (* Note that we're using the versions of new and spawn where they are
       not wired to their continuations. I've experimented with the
       bidirectional rules and it seems that this does not pose any problems. *)
    (* That said, we may revisit this later when we look at deadlock detection. *)
    | New of string (* interface name *)
    | Spawn of expr
    (* interface names for Send and Guard will be added after pre-type checking *)
    | Send of {
        target: expr;
        message: (string * (expr list));
        iname: string option
    }
    | Guard of {
        target: expr;
        (* At least at the moment, each guard must be annotated with a pattern *)
        pattern: (Type.Pattern.t [@name "pattern"]);
        guards: guard list;
        iname: string option
    }
    (* AKA The Diet Guard. free(e), which later desugars to guard e : 1 { free -> () } *)
    | SugarFree of expr
    (* fail(e)[A], desugars to (guard e : 0 { fail } : A) *)
    | SugarFail of expr * (Type.t [@name "ty"])
and constant =
    [%import: Common_types.Constant.t]
and sugar_var = string
and sugar_binder = string
and primitive_name = string
(* Guards are either a receive expression, free, or fail *)
and guard =
    | Receive of {
        tag: string;
        payload_binders: sugar_binder list;
        mailbox_binder: sugar_binder;
        cont: expr
    }
    | Free of expr
    (* For now, require annotation since Fail can have any type *)
    (* It would be nice to get rid of this later. *)
    | Fail of (Type.t[@name "ty"])
and decl = {
    decl_name: string;
    decl_parameters: (string * (Type.t[@name "ty"])) list;
    decl_return_type: (Type.t[@name "ty"]);
    decl_body: expr
}
and program = {
    prog_interfaces: (Interface.t[@name "interface"]) list;
    prog_decls: decl list;
    prog_body: expr option
}
    [@@deriving visitors {
        variety = "map";
        ancestors = ["Type.map"; "Interface.map"];
        data = false } ]

let is_receive_guard = function
    | Receive _ -> true
    | _ -> false

let is_free_guard = function
    | Free _ -> true
    | _ -> false

let is_fail_guard = function
    | Fail _ -> true
    | _ -> false

(* Pretty-printing of the AST *)
(* Programs *)
let rec pp_program ppf { prog_interfaces; prog_decls; prog_body } =
    fprintf ppf "%a@.%a@.@.%a"
        (pp_print_newline_list pp_interface) prog_interfaces
        (pp_print_double_newline_list pp_decl) prog_decls
        (pp_print_option pp_expr) prog_body
(* Interfaces *)
and pp_interface ppf iface =
    let pp_msg_ty ppf (tag, tys) =
        fprintf ppf "%s(%a)" tag
        (pp_print_comma_list Type.pp) tys
    in
    let xs = Interface.bindings iface in
    fprintf ppf "interface %s { %a }"
        (Interface.name iface)
        (pp_print_comma_list pp_msg_ty) xs
(* Declarations *)
and pp_decl ppf { decl_name; decl_parameters; decl_return_type; decl_body } =
    fprintf ppf "def %s(%a): %a {@,@[<v 2>  %a@]@,}"
        decl_name
        (pp_print_comma_list pp_param) decl_parameters
        Type.pp decl_return_type
        pp_expr decl_body
(* Messages *)
and pp_message ppf (tag, es) =
    fprintf ppf "%s(%a)"
        tag
        (pp_print_comma_list pp_expr) es
(* Parameters *)
and pp_param ppf (param, ty) = fprintf ppf "%s: %a" param Type.pp ty
and pp_let_annot ppf ty = fprintf ppf ": %a" Type.pp ty
and pp_bnd_ann ppf (bnd, ann) =
    fprintf ppf
        "%s%a"
        bnd
        pp_let_annot ann
(* Expressions *)
and pp_expr ppf = function
    (* Might want, at some stage, to print out pretype info *)
    | Var x
    | Primitive x -> pp_print_string ppf x
    | Annotate (expr, ty) ->
        fprintf ppf "(%a : %a)" pp_expr expr Type.pp ty
    | Constant c -> Constant.pp ppf c
    | Lam { linear; parameters; body; result_type } ->
        let lin = if linear then "linfun" else "fun" in
        fprintf ppf "%s(%a): %a {@,  @[<v>%a@]@,}"
            lin
            (pp_print_comma_list pp_param) parameters
            Type.pp result_type
            pp_expr body
    | Let { binder; annot; term; body } ->
        fprintf ppf "let %s%a = @[<v>%a@] in@,%a"
            binder
            (pp_print_option pp_let_annot) annot
            pp_expr term
            pp_expr body
    | If { test; then_expr; else_expr } ->
            fprintf ppf "if (%a) {@[<v>%a@]} else {@[<v>%a@]}}"
            pp_expr test
            pp_expr then_expr
            pp_expr else_expr
    | Seq (e1, e2) ->
        fprintf ppf "(%a;@,%a)" pp_expr e1 pp_expr e2
    | App { func; args } ->
        fprintf ppf "%a(%a)"
            pp_expr func
            (pp_print_comma_list pp_expr) args
    | New iname -> fprintf ppf "new[%s]" iname
    | Spawn e -> fprintf ppf "spawn {@[<v>@,%a@]@,}" pp_expr e
    | Send { target; message; _ (* iname *) } ->
        (* Special-case the common case of sending to a variable.
           Bracket the rest for readability. *)
        begin
            match target with
                | Var _ ->
                    fprintf ppf "%a ! %a"
                        pp_expr target
                        pp_message message
                | _ ->
                    fprintf ppf "(@[<v 2>%a@]) ! %a"
                        pp_expr target
                        pp_message message
        end
    | Inl e -> fprintf ppf "inl %a" pp_expr e
    | Inr e -> fprintf ppf "inr %a" pp_expr e
    | Case { term; branch1 = (bnd1, e1); branch2 = (bnd2, e2) } ->
        fprintf
            ppf
            "case %a of {@[@[inl %a -> [@%a@]@][@inr %a -> [@%a@]@]@]}"
            pp_expr term
            pp_bnd_ann bnd1
            pp_expr e1
            pp_bnd_ann bnd2
            pp_expr e2
    | Pair (e1, e2) ->
        fprintf ppf "(%a, %a)" pp_expr e1 pp_expr e2
    | LetPair { binders = (b1, b2); term; cont } ->
        fprintf ppf "let (%s, %s) = %a in %a"
            b1 b2
            pp_expr term
            pp_expr cont
    | Guard { target; pattern; guards; _ } ->
        fprintf ppf
            "guard %a : %a {@,@[<v 2>  %a@]@,}"
            pp_expr target
            Type.Pattern.pp pattern
            (pp_print_newline_list pp_guard) guards
    | SugarFree e -> fprintf ppf "free(%a)" pp_expr e
    | SugarFail (e, ty) -> fprintf ppf "fail(%a)[%a]" pp_expr e Type.pp ty
and pp_guard ppf = function
    | Receive { tag; payload_binders; mailbox_binder; cont } ->
            fprintf ppf "receive %s(%a) from %s ->@,@[<v 2>  %a@]"
            tag
            (pp_print_comma_list pp_print_string) payload_binders
            mailbox_binder
            pp_expr cont
    | Free e ->
        fprintf ppf "free ->@,  @[<v>%a@]" pp_expr e
    | Fail ty ->
        fprintf ppf "fail[%a]" Type.pp ty

(* Probably prettier ways of doing this... *)
let show_program prog = asprintf "%a" pp_program prog
let show_expr expr = asprintf "%a" pp_expr expr

(* Substitutes a pattern solution through the program *)
let substitute_solution sol =
    let visitor =
        object
            inherit [_] map

            method! visit_PatVar _env x =
                match StringMap.find_opt x sol with
                    | Some ty -> ty
                    | None -> Type.Pattern.PatVar x
        end
    in
    visitor#visit_program ()

