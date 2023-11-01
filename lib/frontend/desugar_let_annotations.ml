(*
    let x: A = M in N --> let x = (M : (returnable(A))) in N
 *)
open Common

let visitor =
    object(self)
        inherit [_] Sugar_ast.map as super

        method! visit_expr env =
            let open Sugar_ast in
            function
                | Let { binder; annot = Some annot'; term; body } ->
                    let inner_term = self#visit_expr env term in
                    let body = self#visit_expr env body in
                    let term = Annotate (inner_term, Type.make_returnable annot') in
                    Let { binder; annot = None; term; body }
                | LetPair { binders; annot = Some (ty1, ty2); term; cont } ->
                    let cont = self#visit_expr env cont in
                    let term =
                        Annotate (term,
                                  Type.make_returnable (Type.make_pair_type ty1 ty2))
                    in
                    LetPair { binders; annot = None; term; cont }
                | e -> super#visit_expr env e
    end

let desugar =
    visitor#visit_program ()
