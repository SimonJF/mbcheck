open Common_types
open Util.Utility

type t = { name: string; env: (string * (Type.t[@name "ty"]) list) list }
[@@name "interface"]
[@@deriving visitors {
    variety = "map";
    ancestors = ["Type.map"];
    data = false }]

let lookup (x: tag) (iface: t) =
    match List.assoc_opt x iface.env with
        | Some tys -> tys
        | None ->
            let msg =
                Printf.sprintf
                    "Message tag %s not supported by interface %s."
                    x iface.name
            in
            raise (Errors.Type_error msg)

let make name env =
    { name; env }

let name x = x.name

let bindings x = x.env

let pp ppf x =
    let open Format in
    let pp_message ppf (name, tys) =
        fprintf ppf "%a(%a)"
            pp_print_string name
            (pp_print_comma_list Type.pp) tys
    in
    fprintf ppf "interface %a { %a }"
        pp_print_string x.name
        (pp_print_comma_list pp_message)
        x.env
