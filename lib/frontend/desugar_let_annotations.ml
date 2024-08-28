(*
    let x: A = M in N --> let x = (M : (returnable(A))) in N
 *)
open Common

let visitor =
    object(self)
        inherit [_] Sugar_ast.map as super

        method! visit_expr env expr_with_pos =
            let open Sugar_ast in
            let open Source_code in
            let expr_node = WithPos.node expr_with_pos in
            match expr_node with
                | Let { binder; annot = Some annot'; term; body } ->
                    let inner_term = self#visit_expr env term in
                    let body = self#visit_expr env body in
                    let term = { expr_with_pos with node = Annotate (inner_term, Type.make_returnable annot') } in
                        { expr_with_pos with node = Let { binder; annot = None; term; body } }
                | LetPair { binders; annot = Some (ty1, ty2); term; cont } ->
                    let cont = self#visit_expr env cont in
                    let term =
                        { expr_with_pos with node = 
                            Annotate (term, Type.make_returnable (Type.make_pair_type ty1 ty2)) }
                    in
                        { expr_with_pos with node = LetPair { binders; annot = None; term; cont } }
                | _ -> super#visit_expr env expr_with_pos
    end

let desugar =
    visitor#visit_program ()
