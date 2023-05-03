(* Random stuff that's useful everywhere *)

(* Maps *)

module type STRINGMAP = (Map.S with type key = string)
module StringMap = Map.Make(String)
type 'a stringmap = 'a StringMap.t

module type STRINGSET = (Set.S with type elt = string)
module StringSet = Set.Make(String)
type stringset = StringSet.t

(* Pipelining and composition *)

(* Reverse function application (nicer and more uniform than `@@`) *)
let (<|) f x = f x

(* Function composition (left) *)
let (<<) f g x = f(g(x))

(* Function composition (right) *)
let (>>) f g x = g(f(x))

let flip f = fun x y -> f y x

(* Chars *)

let is_uppercase c =
    let code = Char.code c in
    code >= Char.code 'A' && code <= Char.code 'Z'

(* Lists *)

module ListUtils = struct

let rec split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list = function
    | [] -> ([], [], [])
    | (x, y, z) :: rest ->
        let (xs, ys, zs) = split3 rest in
        x :: xs, y :: ys, z :: zs

let rec combine3 : 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list =
    fun xs ys zs ->
        match (xs, ys, zs) with
            | ([], [], []) -> []
            | (x :: xs, y :: ys, z :: zs) ->
                let rest = combine3 xs ys zs in
                (x, y, z) :: rest
            | _, _, _ ->
                raise (Invalid_argument "mismatching lengths to combine3")
end

(* Pretty-printing *)
open Format
let pp_comma ppf () =
    pp_print_string ppf ", "

let pp_print_comma_list ppf =
    pp_print_list ~pp_sep:(pp_comma) ppf

let pp_print_newline_list ppf =
    pp_print_list ~pp_sep:(pp_force_newline) ppf

let pp_double_newline ppf () =
    pp_force_newline ppf ();
    pp_force_newline ppf ()

let pp_print_double_newline_list ppf =
    pp_print_list ~pp_sep:(pp_double_newline) ppf

(* Prints an error *)
let print_error ?(note="ERROR") err =
    Format.fprintf err_formatter "[\027[31m%s\027[0m] %s\n" note err

let print_debug err =
    Format.fprintf std_formatter "[\027[34mDEBUG\027[0m] %s\n" err

