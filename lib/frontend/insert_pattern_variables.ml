(* Annotation *)
(* Annotation takes partially-defined mailbox types (i.e., MB types with
 * interface information but no pattern annotations) and annotates them with
 * fresh pattern variables so that we can generate constraints.
 * This is done for all types that are in binding position.  *)
(* Additionally, for interfaces, sets the quasilinearity to Usable. *)
open Common
open Sugar_ast
open Source_code

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
        | Tuple ts ->
            Tuple (List.map annotate_type ts)
        | Sum (t1, t2) ->
            Sum (annotate_type t1, annotate_type t2)
        | List t ->
            List (annotate_type t)
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

        method! visit_expr env expr_with_pos =
            let open Sugar_ast in
            let expr = WithPos.node expr_with_pos in
            match expr with
            | Annotate (e, ty) ->
                let new_e = self#visit_expr env e in
                let new_annotate = Annotate (new_e, annotate_type ty) in
                { expr_with_pos with node = new_annotate }
            | Lam { linear; parameters; result_type; body } ->
                let parameters =
                    List.map (fun (x, y) -> (x, annotate_type y)) parameters in
                let result_type = annotate_type result_type in
                let new_body = self#visit_expr env body in
                let new_lam = Lam { linear; parameters; result_type; body = new_body } in
                { expr_with_pos with node = new_lam }
            | _ -> super#visit_expr env expr_with_pos

        method! visit_program env p =
            let prog_interfaces =
                List.map annotate_interface (WithPos.extract_list_node p.prog_interfaces) in
            let prog_interfaces_with_pos =
                List.map2 (fun iface pos -> WithPos.make ~pos iface) prog_interfaces (List.map WithPos.pos p.prog_interfaces) in
            let prog_decls =
                let (poses, nodes) = WithPos.split_with_pos_list p.prog_decls in
                let visited_nodes = self#visit_list (self#visit_decl) env nodes in
                WithPos.combine_with_pos_list poses visited_nodes in
            let prog_body =
                self#visit_option (self#visit_expr) env p.prog_body in
            { prog_interfaces = prog_interfaces_with_pos; prog_decls; prog_body }

    end

let annotate prog =
    let prog = visitor#visit_program () prog in
    Settings.if_verbose (fun () ->
        Format.(fprintf std_formatter "=== Annotated Program ===\n%a\n\n"
        Sugar_ast.pp_program prog));
    prog
