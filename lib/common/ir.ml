(* FGCBV IR *)
open Common_types
open Format
open Util.Utility

module Binder = struct
    type t = { id: int; name: string }
    [@@name "binder"]
    [@@deriving visitors { variety = "map"; data = false }]

    (* Accessors *)
    let id x = x.id
    let name x = x.name

    let source = ref 0

    let gen () =
        let res = !source in
        incr source;
        res

    let make ?(name="") () =
        { id = gen (); name = name }

    (* Display *)
    let pp ppf x =
        let prefix =
            if x.name = "" then "_" else x.name in
        Format.pp_print_string ppf (prefix ^ (string_of_int x.id))
end

module Var = struct
    type t = { id: int; name: string }
    [@@name "var"]
    [@@deriving visitors { variety = "map"; data = false }]

    (* Accessors *)
    let id x = x.id
    let name x = x.name

    (* Display *)
    let pp ppf x =
        let prefix =
            if x.name = "" then "_" else x.name in
        Format.pp_print_string ppf (prefix ^ (string_of_int x.id))

    let pp_name ppf x =
        Format.pp_print_string ppf x.name

    let unique_name =
        Format.asprintf "%a" pp

    let of_binder : Binder.t -> t = fun x ->
        { id = Binder.id x; name = Binder.name x }

    let compare x1 x2 =
        compare (unique_name x1) (unique_name x2)
end


type program = {
    prog_interfaces: (Interface.t[@name "interface"]) list;
    prog_decls: decl list;
    prog_body: comp option
}
and decl = {
    decl_name: (Binder.t[@name "binder"]);
    decl_parameters: ((Binder.t[@name "binder"]) * (Type.t[@name "ty"])) list;
    decl_return_type: (Type.t[@name "ty"]);
    decl_body: comp
}
and comp =
    | Annotate of comp * (Type.t[@name "ty"])
    | Let of {
        binder: (Binder.t[@name "binder"]);
        term: comp;
        cont: comp
      }
    | Seq of (comp * comp)
    | Return of value
    | App of {
        func: value;
        args: value list
      }
    | If of { test: value; then_expr: comp; else_expr: comp }
    | LetPair of {
        binders: ((Binder.t[@name "binder"]) * (Binder.t[@name "binder"]));
        pair: value;
        cont: comp
    }
    | Case of {
        term: value;
        branch1: ((Binder.t[@name "binder"]) * (Type.t[@name "ty"])) * comp;
        branch2: ((Binder.t[@name "binder"]) * (Type.t[@name "ty"])) * comp
    }
    | New of string
    | Spawn of comp
    | Send of {
        target: value;
        message: (message[@name "msg"]);
        iname: string option
      }
    | Guard of {
        target: value;
        pattern: (Type.Pattern.t[@name "pattern"]);
        guards: guard list;
        iname: string option
      }
and value =
    | VAnnotate of value * (Type.t[@name "ty"])
    | Constant of constant
    | Primitive of primitive_name
    | Variable of (Var.t[@name "var"]) * (Pretype.t[@name "pretype"]) option
    | Pair of value * value
    | Inl of value
    | Inr of value
    | Lam of {
        linear: bool;
        parameters: ((Binder.t[@name "binder"]) * (Type.t[@name "ty"])) list;
        result_type: (Type.t[@name "ty"]);
        body: comp
    }
and message = (string * value list)
    [@@name "msg"]
and primitive_name = string
and constant =
    [%import: Common_types.Constant.t]
and guard =
    | Receive of {
        tag: string;
        payload_binders: (Binder.t[@name "binder"]) list;
        mailbox_binder: (Binder.t[@name "binder"]);
        cont: comp
    }
    | Free of comp
    | Fail
    [@@deriving visitors {
        variety = "map";
        ancestors = [
            "Type.map"; "Pretype.map"; "Binder.map";
            "Interface.map"; "Var.map"];
        data = false },
    show]

(* Pretty-printing of the AST *)
(* Programs *)
let rec pp_program ppf { prog_interfaces; prog_decls; prog_body } =
    fprintf ppf "%a@.%a@.@.%a"
        (pp_print_newline_list pp_interface) prog_interfaces
        (pp_print_double_newline_list pp_decl) prog_decls
        (pp_print_option pp_comp) prog_body
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
    fprintf ppf "def %a(%a): %a {@,@[<v 2>  %a@]@,}"
        Binder.pp decl_name
        (pp_print_comma_list pp_param) decl_parameters
        Type.pp decl_return_type
        pp_comp decl_body
(* Messages *)
and pp_message ppf (tag, vs) =
    fprintf ppf "%s(%a)"
        tag
        (pp_print_comma_list pp_value) vs
(* Parameters *)
and pp_param ppf (param, ty) = fprintf ppf "%a: %a" Binder.pp param Type.pp ty
and pp_branch name ppf ((bnd, ty), c) =
    fprintf ppf "%s(%a): %a -> @[<v>%a@]"
        name
        Binder.pp bnd
        Type.pp ty
        pp_comp c
(* Expressions *)
and pp_comp ppf = function
    | Annotate (c, ty) ->
        fprintf ppf "(%a : %a)" pp_comp c Type.pp ty
    | Seq (c1, c2) ->
        fprintf ppf "(%a;@,%a)" pp_comp c1 pp_comp c2
    | Let { binder; term; cont } ->
        fprintf ppf "let %a = @[<v>%a@] in@,%a"
            Binder.pp binder
            pp_comp term
            pp_comp cont
    | Return v ->
            pp_value ppf v
    | If { test; then_expr; else_expr } ->
            fprintf ppf "if (%a) {@[<v>%a@]} else {@[<v>%a@]}}"
            pp_value test
            pp_comp then_expr
            pp_comp else_expr
    | App { func; args } ->
        fprintf ppf "%a(%a)"
            pp_value func
            (pp_print_comma_list pp_value) args
    | New iname -> fprintf ppf "new[%s]" iname
    | Spawn e -> fprintf ppf "spawn {@[<v>@,%a@]@,}" pp_comp e
    | Send { target; message; _ (* iname *) } ->
        (* Special-case the common case of sending to a variable.
           Bracket the rest for readability. *)
        begin
            match target with
                | Variable _ ->
                    fprintf ppf "%a ! %a"
                        pp_value target
                        pp_message message
                | _ ->
                    fprintf ppf "(@[<v 2>%a@]) ! %a"
                        pp_value target
                        pp_message message
        end
    | LetPair { binders = (b1, b2); pair; cont } ->
        fprintf ppf "let (%a, %a) = @[<v>%a@] in@,%a"
            Binder.pp b1
            Binder.pp b2
            pp_value pair
            pp_comp cont
    | Case { term; branch1; branch2 } ->
        fprintf ppf
            "case %a of {@[<v>@[<v>%a@]@,@[<v>%a@]@]}"
            pp_value term
            (pp_branch "inl") branch1
            (pp_branch "inr") branch2
    | Guard { target; pattern; guards; _ } ->
        fprintf ppf
            "guard %a : %a {@,@[<v 2>  %a@]@,}"
            pp_value target
            Type.Pattern.pp pattern
            (pp_print_newline_list pp_guard) guards
and pp_value ppf = function
    (* Might want, at some stage, to print out pretype info *)
    | VAnnotate (value, ty) ->
        fprintf ppf "(%a : %a)" pp_value value Type.pp ty
    | Primitive prim -> Format.pp_print_string ppf prim
    | Variable (var, _) -> Var.pp ppf var
    | Constant c -> Constant.pp ppf c
    | Pair (v1, v2) ->
        fprintf ppf "(%a, %a)" pp_value v1 pp_value v2
    | Inl v -> fprintf ppf "inl(%a)" pp_value v
    | Inr v -> fprintf ppf "inr(%a)" pp_value v
    | Lam { linear; parameters; result_type; body } ->
        let lin = if linear then "linfun" else "fun" in
        fprintf ppf "%s(%a): %a {@,  @[<v>%a@]@,}"
            lin
            (pp_print_comma_list pp_param) parameters
            Type.pp result_type
            pp_comp body
and pp_guard ppf = function
    | Receive { tag; payload_binders; mailbox_binder; cont } ->
            fprintf ppf "receive %s(%a) from %a ->@,@[<v 2>  %a@]"
            tag
            (pp_print_comma_list Binder.pp) payload_binders
            Binder.pp mailbox_binder
            pp_comp cont
    | Free e ->
        fprintf ppf "free ->@,  @[<v>%a@]" pp_comp e
    | Fail ->
        fprintf ppf "fail"


let is_receive_guard = function
    | Receive _ -> true
    | _ -> false

let is_free_guard = function
    | Free _ -> true
    | _ -> false

let is_fail_guard = function
    | Fail -> true
    | _ -> false


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

