(*
    free(M)    ---> guard e : 1 { free -> () }
    fail(M)[A] ---> guard e : 0 { fail[A] }
 *)
open Common
open Common_types

let visitor =
    object(self)
        inherit [_] Sugar_ast.map as super

        method! visit_expr env =
            let open Sugar_ast in
            function
                | SugarFree e ->
                    Guard {
                        target = self#visit_expr env e;
                        pattern = Type.Pattern.One;
                        guards = [Free (Constant Constant.unit)];
                        iname = None
                    }
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
