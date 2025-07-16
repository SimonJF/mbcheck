(* Pre-types: purely statically-determinable types which do not require
   behavioural typing or any non-trivial inference. Pre-types are either base
   types or interface annotations. Note that they do *not* include any message
   sequencing information.

   Basically lambda-act with first-class mailboxes.
*)
open Util.Utility

type t =
    | PBase of base
    | PVar of string
    | PFun of { linear: bool; typarams: (Type.t[@name "ty"]) list; args: (Type.t[@name "ty"]) list; result: (Type.t[@name "ty"]) }
    | PInterface of (string * (Type.t[@name "ty"]) list)
    | PSum of (t * t)
    | PTuple of t list
    [@@name "pretype"]
    [@@deriving visitors { variety = "map" }]
and base = [%import: Common_types.Base.t]

let unit = PTuple []

let rec pp ppf =
  let open Format in
  function
    | PBase b -> Common_types.Base.pp ppf b
    | PVar s -> fprintf ppf "%s" s
    | PFun { linear; typarams; args; result } ->
        let arrow = if linear then "-o" else "->" in
        fprintf ppf "<%a> (%a) %s %a"
            (pp_print_list Type.pp) typarams
            (pp_print_comma_list Type.pp) args
            arrow
            Type.pp result
    | PTuple ts ->
        let pp_star ppf () = pp_print_string ppf " * " in
        fprintf ppf "(%a)"
            (pp_print_list ~pp_sep:(pp_star) pp) ts
    | PSum (t1, t2) ->
        fprintf ppf "(%a + %a)"
            pp t1
            pp t2
    | PInterface (iname, tyargs) ->
        fprintf ppf "%s<%a>"
            iname
            (pp_print_list Type.pp) tyargs

let show t =
  let open Format in
  pp str_formatter t;
  flush_str_formatter ()

let rec of_type = function
    | Type.Base b -> PBase b
    | Type.TVar s -> PVar s
    | Type.Fun { linear; typarams; args; result } ->
        PFun { linear; typarams; args; result = result }
    | Type.Tuple ts -> PTuple (List.map of_type ts)
    | Type.Sum (t1, t2) -> PSum (of_type t1, of_type t2)
    | Type.Mailbox { interface; _ } -> PInterface interface

(* As long as a pretype isn't a mailbox type, and isn't a function
    returning a mailbox type, we can upgrade it to a type.
    This reduces the number of places we need annotations (for example,
    when trying to type an application in synthesis mode). *)
let rec to_type = function
    | PBase b -> Some (Type.Base b)
    | PVar s -> Some (Type.TVar s)
    | PFun { linear; typarams; args; result } ->
        Some (Type.Fun { linear; typarams; args; result })
    | PTuple ts ->
        let rec go acc =
            function
                | [] -> Some (List.rev acc)
                | x :: xs ->
                    Option.bind (to_type x) (fun t -> go (t :: acc) xs)
        in
        Option.bind (go [] ts) (fun ts -> Some (Type.Tuple ts))
    | PSum (t1, t2) ->
        begin
            match to_type t1, to_type t2 with
                | Some ty1, Some ty2 -> Some (Type.Sum (ty1, ty2))
                | _, _ -> None
        end
    | PInterface _ -> None
