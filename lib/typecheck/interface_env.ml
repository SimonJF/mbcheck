open Util.Utility
open Common
open Common.Source_code

type t = (Interface.t WithPos.t) StringMap.t

let lookup x (env: t) pos_list =
    match StringMap.find_opt x env with
        | Some x -> x
        | None -> raise (Errors.type_error ("No such interface " ^ x) pos_list)

let bind = StringMap.add

let bind_many =
    List.fold_right (fun (v, prety) acc ->
        StringMap.add v prety acc)

let from_list (xs : (Interface.t WithPos.t) list) : t =
    let xs = List.map (fun x -> (Interface.name (WithPos.node x), x)) xs in
    bind_many xs StringMap.empty
