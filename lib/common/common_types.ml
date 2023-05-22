(* Common types used over multiple files *)

module Base = struct
    type t =
        | Int
        | Bool
        | String
        | Unit
    [@@deriving show]
end

type interface_name = string
    [@@name "string"]
    [@@deriving show]

type tag = string
    [@@name "string"]
    [@@deriving show]

(* Boxed constants *)
module Constant = struct
    type t =
        | Unit
        | Int of int
        | String of string
        | Bool of bool

    let type_of = function
        | Unit -> Base.Unit
        | Int _ -> Base.Int
        | Bool _ -> Base.Bool
        | String _ -> Base.String

    let pp ppf = function
        | Unit -> Format.pp_print_string ppf "()"
        | Int i -> Format.pp_print_int ppf i
        | String s -> Format.fprintf ppf "\"%s\"" s
        | Bool b -> Format.pp_print_bool ppf b

    let unit = Unit
    let wrap_int i = Int i
    let wrap_string s = String s
    let wrap_bool b = Bool b

    let unwrap_int = function
        | Int i -> i
        | _ -> raise (Errors.type_error "unwrap_int on non-int")

    let unwrap_string = function
        | String s -> s
        | _ -> raise (Errors.type_error "unwrap_string on non-string")

    let unwrap_bool = function
        | Bool b -> b
        | _ -> raise (Errors.type_error "unwrap_bool on non-bool")
end
