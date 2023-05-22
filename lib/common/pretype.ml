(* Pre-types: purely statically-determinable types which do not require
   behavioural typing or any non-trivial inference. Pre-types are either base
   types or interface annotations. Note that they do *not* include any message
   sequencing information.

   Basically lambda-act with first-class mailboxes.
*)
open Util.Utility

type t =
    | PBase of base
    (* Functions are always annotated with argument types.
       The codomain is a pretype, since it is not in binding position. *)
    | PFun of { linear: bool; args: (Type.t[@name "ty"]) list; result: t[@name "pretype"] }
    | PInterface of string
    | PSum of (t * t)
    | PPair of (t * t)
    [@@name "pretype"]
    [@@deriving visitors { variety = "map" }]
and base = [%import: Common_types.Base.t]

let rec pp ppf =
  let ps = Format.pp_print_string ppf in
  function
    | PBase b ->
       begin
         match b with
           | Unit -> ps "Unit"
           | Int -> ps "Int"
           | Bool -> ps "Bool"
           | String -> ps "String"
       end
    | PFun { linear; args; result } ->
        let arrow = if linear then "-o" else "->" in
        Format.fprintf ppf "(%a) %s %a"
            (pp_print_comma_list Type.pp) args
            arrow
            pp result
    | PPair (t1, t2) ->
        Format.fprintf ppf "(%a * %a)"
            pp t1
            pp t2
    | PSum (t1, t2) ->
        Format.fprintf ppf "(%a + %a)"
            pp t1
            pp t2
    | PInterface name -> ps name

let show t =
  let open Format in
  pp str_formatter t;
  flush_str_formatter ()

let rec of_type = function
    | Type.Base b -> PBase b
    | Type.Fun { linear; args; result } ->
        PFun { linear; args; result = of_type result }
    | Type.Pair (t1, t2) -> PPair (of_type t1, of_type t2)
    | Type.Sum (t1, t2) -> PSum (of_type t1, of_type t2)
    | Type.Mailbox { interface; _ } -> PInterface interface

(* As long as a pretype isn't a mailbox type, and isn't a function
    returning a mailbox type, we can upgrade it to a type.
    This reduces the number of places we need annotations (for example,
    when trying to type an application in synthesis mode). *)
let rec to_type = function
    | PBase b -> Some (Type.Base b)
    | PFun { linear; args; result } ->
        Option.bind (to_type result)
        (fun result ->
            Some (Type.Fun { linear; args; result })
        )
    | PPair (t1, t2) ->
        begin
        match to_type t1, to_type t2 with
            | Some ty1, Some ty2 -> Some (Type.Pair (ty1, ty2))
            | _, _ -> None
        end
    | PSum (t1, t2) ->
        begin
        match to_type t1, to_type t2 with
            | Some ty1, Some ty2 -> Some (Type.Sum (ty1, ty2))
            | _, _ -> None
        end
    | PInterface _ -> None
