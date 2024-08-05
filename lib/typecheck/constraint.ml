(* A constraint is a language inclusion relation between two patterns. *)
open Common
open Type
open Common.SourceCode

type t = (Pattern.t * Pattern.t) WithPos.t

exception Trap of string

let make p1 p2 pos = WithPos.make ~pos (p1, p2)

let lhs { WithPos.node = (p1, _); _ } = p1
let rhs { WithPos.node = (_, p2); _ } = p2

let pos { WithPos.pos; _ } = pos

let is_upper_bound =
    let open Pattern in
    function
        | { WithPos.node = (_, PatVar _); _ } -> true
        | _ -> false

let is_lower_bound { WithPos.node = (_, p2); _ } = Pattern.defined p2

(* Use default comparison function to allow set equality *)
let compare = compare

let pp ppf { WithPos.node = (p1, p2); _ } =
    Format.(fprintf ppf "%a ⊑ %a"
        Pattern.pp p1
        Pattern.pp p2)
