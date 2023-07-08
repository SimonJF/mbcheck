open Util.Utility
open Common

type t = Interface.t StringMap.t

let lookup x (env: t) =
    match StringMap.find_opt x env with
        | Some x -> x
        | None -> raise (Errors.type_error ("No such interface " ^ x))

let bind = StringMap.add

let bind_many =
    List.fold_right (fun (v, prety) acc ->
        StringMap.add v prety acc)

let from_list (xs : Interface.t list) : t =
    let xs = List.map (fun x -> (Interface.name x, x)) xs in
    bind_many xs StringMap.empty
