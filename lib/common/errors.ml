open Util

(* Auxiliary definitions / types *)
type subsystem =
    | GenSubtype
    | GenIntersect
    | GenJoin
    | GenCombine
    | GenSynth
    | GenCheck
    | GenCheckGuard
    | GenCheckDecls

let show_subsystem = function
    | GenSubtype -> "Subtype"
    | GenJoin -> "Join"
    | GenCombine -> "Combine"
    | GenIntersect -> "Intersect"
    | GenSynth -> "Synth"
    | GenCheck -> "Check"
    | GenCheckGuard -> "Check guard"
    | GenCheckDecls -> "Check declarations"

(* Exceptions *)
exception Parse_error of string
exception Pretype_error of string
exception Type_error of string (* Used for errors common to both pretyping and constraint generation *)
exception Constraint_gen_error of { subsystem: subsystem option; message: string }
(* It would be a bit nicer to have the constraints on the LHS and RHS here,
   but it would introduce a cyclic library dependency. *)
exception Constraint_solver_error of { lhs: string; rhs: string }
exception Constraint_solver_zero_error of string
exception Internal_error of { filename: string; message: string }
exception Bad_receive_typing_argument of string
exception Transform_error of string


let internal_error filename message = Internal_error { filename; message }
let parse_error msg = Parse_error msg
let type_error message = Type_error message
let constraint_solver_error lhs rhs = Constraint_solver_error { lhs; rhs }
let constraint_solver_zero_error var = Constraint_solver_zero_error var
let bad_receive_typing_argument bad = Bad_receive_typing_argument bad
let transform_error err = Transform_error err

(* Will likely be more interesting when we have positional information *)
let format_error = function
    | Internal_error { filename; message } ->
        let note = Printf.sprintf "INTERNAL (%s)" filename in
        Utility.print_error ~note message
    | Parse_error s ->
        Utility.print_error ~note:"PARSE" s
    | Pretype_error s ->
        Utility.print_error ~note:"PRETYPE" s
    | Transform_error s ->
        Utility.print_error ~note:"TRANSFORM" s
    | Type_error s ->
        Utility.print_error ~note:"TYPE" s
    | Constraint_gen_error { subsystem; message } ->
        let note =
            match subsystem with
                | Some subsystem ->
                    Printf.sprintf
                        "CONSTRAINT GENERATION (%s)"
                        (show_subsystem subsystem)
                | None -> "CONSTRAINT GENERATION"
        in
        Utility.print_error ~note message
    | Constraint_solver_error { lhs; rhs } ->
        let msg =
            Printf.sprintf
                "%s is not included in %s"
                lhs rhs
        in
        Utility.print_error ~note:"CONSTRAINT SOLVING" msg
    | Constraint_solver_zero_error var ->
        let msg =
            Printf.sprintf
                "Pattern variable %s was solved as pattern 0. This can happen when only part of a program is written. Consider finishing the program or adding a type annotation."
                var
        in
        Utility.print_error ~note:"CONSTRAINT SOLVING" msg
    | Bad_receive_typing_argument bad ->
        Printf.sprintf
            "%s is not a valid receive typing strategy (allowed: strict, interface, none)"
            bad
        |> Utility.print_error
    | e ->
        Utility.print_error (Printexc.to_string e)

