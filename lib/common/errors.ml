open Util
open SourceCode

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
exception Parse_error of string * Position.t list
exception Pretype_error of string * Position.t list
exception Type_error of string * Position.t list (* Used for errors common to both pretyping and constraint generation *)
exception Constraint_gen_error of { subsystem: subsystem option; message: string; pos_list: Position.t list  }
(* It would be a bit nicer to have the constraints on the LHS and RHS here,
   but it would introduce a cyclic library dependency. *)
exception Constraint_solver_error of { lhs: string; rhs: string; pos_list: Position.t list  }
exception Constraint_solver_zero_error of string
exception Internal_error of { filename: string; message: string }
exception Bad_receive_typing_argument of string
exception Transform_error of string * Position.t list


let internal_error filename message = Internal_error { filename; message }
let parse_error msg pos_list = Parse_error (msg, pos_list)
let type_error message pos_list = Type_error (message, pos_list)
let constraint_solver_error lhs rhs pos_list = Constraint_solver_error { lhs; rhs; pos_list }
let constraint_solver_zero_error var = Constraint_solver_zero_error var
let bad_receive_typing_argument bad = Bad_receive_typing_argument bad
let transform_error err pos_list = Transform_error (err, pos_list)

(* Will likely be more interesting when we have positional information *)
let format_error = function
    | Internal_error { filename; message } ->
        let note = Printf.sprintf "INTERNAL (%s)" filename in
        Utility.print_error ~note message
    | Parse_error (s, pos_list) ->
        let pos_info = Position.format_pos pos_list in
        Utility.print_error ~note:"PARSE" (s ^ " \n " ^ pos_info)
    | Pretype_error (s, pos_list) ->
        let pos_info = Position.format_pos pos_list in
        Utility.print_error ~note:"PRETYPE" (s ^ " \n " ^ pos_info)
    | Transform_error (s, pos_list) ->
        let pos_info = Position.format_pos pos_list in
        Utility.print_error ~note:"TRANSFORM" (s ^ " \n " ^ pos_info)
    | Type_error (s, pos_list) ->
        let pos_info = Position.format_pos pos_list in
        Utility.print_error ~note:"TYPE" (s ^ " \n " ^ pos_info)
    | Constraint_gen_error { subsystem; message; pos_list } ->
        let note =
            match subsystem with
                | Some subsystem ->
                    Printf.sprintf
                        "CONSTRAINT GENERATION (%s)"
                        (show_subsystem subsystem)
                | None -> "CONSTRAINT GENERATION"
        in
        let pos_info = Position.format_pos pos_list in
        Utility.print_error ~note (message ^ " \n " ^ pos_info)
    | Constraint_solver_error { lhs; rhs; pos_list } ->
        let msg =
            Printf.sprintf
                "%s is not included in %s"
                lhs rhs
        in
        let pos_info = Position.format_pos pos_list in
        Utility.print_error ~note:"CONSTRAINT SOLVING" (msg ^ " \n " ^ pos_info)
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

