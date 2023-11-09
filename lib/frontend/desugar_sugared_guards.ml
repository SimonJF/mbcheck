(*
    fail(M)[A] ---> guard e : 0 { fail[A] }
 *)
open Common

let visitor =
    object(self)
        inherit [_] Sugar_ast.map as super

        method! visit_guard env =
            let open Sugar_ast in
            function
                | GFree e ->
                    let var = "_gf" in
                    let e = self#visit_expr env e in
                    Empty (var, Seq (Free (Var var), e))
                | g -> super#visit_guard env g

        method! visit_expr env =
            let open Sugar_ast in
            function
                | SugarFail (e, ty) ->
                   Annotate (
                       Guard {
                            target = self#visit_expr env e;
                            pattern = Type.Pattern.Zero;
                            guards = [Fail ty];
                            iname = None
                        }, ty)
                | e -> super#visit_expr env e
    end

let desugar =
    visitor#visit_program ()
