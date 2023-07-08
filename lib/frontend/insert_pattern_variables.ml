(* Annotation *)
(* Annotation takes partially-defined mailbox types (i.e., MB types with
 * interface information but no pattern annotations) and annotates them with
 * fresh pattern variables so that we can generate constraints.
 * This is done for all types that are in binding position.  *)
(* Additionally, for interfaces, sets the quasilinearity to Usable. *)
open Common
open Sugar_ast

let rec annotate_type =
    let open Type in
    function
        | Base t -> Base t
        | Fun { linear; args; result } ->
            Fun {
                linear;
                args = List.map annotate_type args;
                result = annotate_type result
            }
        | Pair (t1, t2) ->
            Pair (annotate_type t1, annotate_type t2)
        | Sum (t1, t2) ->
            Sum (annotate_type t1, annotate_type t2)
        | Mailbox { pattern = Some _; _ } as mb -> mb
        | Mailbox { capability; interface; pattern = None; quasilinearity } ->
            Mailbox {
                capability;
                interface;
                pattern = Some (Pattern.fresh ());
                quasilinearity
            }

let annotate_interface_type =
    let open Type in
    function
        (* Outermost MB types (i.e., payloads) are treated as usable. *)
        | Mailbox { pattern = Some _; _ } as mb -> mb
        | Mailbox { capability; interface; pattern = None; _ } ->
            Mailbox {
                capability;
                interface;
                pattern = Some (Pattern.fresh ());
                quasilinearity = Quasilinearity.Usable
            }
        | t -> annotate_type t

(* Annotates all types in an interface *)
let annotate_interface iface =
    Interface.bindings iface
    |> List.map (fun (tag, tys) -> (tag, List.map annotate_interface_type tys))
    |> Interface.(make (name iface))

(* The visitor traverses the AST to annotate parameters of higher-order
   functions. *)
let visitor =
    object(self)
        inherit [_] Sugar_ast.map as super

        method! visit_decl env decl =
            { decl with
                decl_parameters =
                    List.map (fun (x, t) -> (x, annotate_type t))
                        decl.decl_parameters;
                decl_return_type = annotate_type decl.decl_return_type;
                decl_body = self#visit_expr env decl.decl_body }


        method! visit_expr env = function
            | Lam { linear; parameters; result_type; body } ->
                let parameters =
                    List.map (fun (x, y) -> (x, annotate_type y)) parameters in
                let result_type = annotate_type result_type in
                let body = self#visit_expr env body in
                Lam { linear; parameters; result_type; body }
            | e -> super#visit_expr env e

        method! visit_program env p =
            let prog_interfaces =
                List.map annotate_interface p.prog_interfaces in
            let prog_decls =
                self#visit_list (self#visit_decl) env p.prog_decls in
            let prog_body =
                self#visit_option (self#visit_expr) env p.prog_body in
            { prog_interfaces; prog_decls; prog_body }

    end

let annotate prog =
    let prog = visitor#visit_program () prog in
    Settings.if_verbose (fun () ->
        Format.(fprintf std_formatter "=== Annotated Program ===\n%a\n\n"
        Sugar_ast.pp_program prog));
    prog

