open Common
open Eval

let print_result (prog, prety, _ir, ty, _env, _constrs) =
    let open Format in
    Settings.if_verbose (fun () ->
        (* Print Program *)
        Format.printf
            "=== Resolved Program: ===\n%a\n\n"
            (Sugar_ast.pp_program) prog;
        (* Print pretype *)
        Option.iter
            (fprintf std_formatter "Pretype: %a\n" Pretype.pp) prety;
        (* Print type *)
        fprintf std_formatter "Type: %a\n" Type.pp ty
    )

(* for IR translation testing *)
let print_ir (prog, _prety, ir, _ty, _env, _constrs) =
    Format.printf
        "=== Resolved Program: ===\n%a\n\n"
        (Sugar_ast.pp_program) prog;
    Format.printf
        "=== Intermediate Representation: ===\n%a\n\n"
        (Ir.pp_program) ir


let print_generated result =
    Eval.Steps_printer.print_value result

let process filename is_verbose is_debug is_ir mode benchmark_count () =
    Settings.(set verbose is_verbose);
    Settings.(set debug is_debug);
    Settings.(set receive_typing_strategy mode);
    Settings.(set benchmark benchmark_count);
    try
        let temp = Frontend.Parse.parse_file filename ()
            |> Frontend.Pipeline.pipeline in
        let (_, _, ir_program, _, _, _) = temp in
        let result = Generator.generate ir_program in
        print_generated result;
        if is_ir then 
            print_ir temp
        else 
            print_result temp;
    with
        | e ->
            Errors.format_error e;
            Settings.if_debug (fun () -> Printexc.print_backtrace stderr);
            (exit (1))

let () =
  let open Cmdliner in
  let mbcheck_t = Term.(const process
    $ Arg.(required & pos 0 (some string) None & info [] ~docv:"FILENAME")
    $ Arg.(value & flag & info ["v"; "verbose"] ~doc:"verbose typechecking information")
    $ Arg.(value & flag & info ["d"; "debug"] ~doc:"print debug information")
    $ Arg.(value & flag & info ["ir"] ~doc:"print the parsed program and its IR translation")
    $ Arg.(value & opt (enum Settings.ReceiveTypingStrategy.enum) Settings.ReceiveTypingStrategy.Interface & info ["mode"]
      ~docv:"MODE" ~doc:"typechecking mode for receive blocks (allowed: strict, interface, none)")
    $ Arg.(value & opt int (-1) & info ["b"; "benchmark"]
      ~docv:"BENCHMARK" ~doc:"number of repetitions for benchmark; -1 (default) for no benchmarking")
    $ const ()) in
  let info = Cmd.info "mbcheck" ~doc:"Typechecker for mailbox calculus" in
  Cmd.v info mbcheck_t
  |> Cmd.eval
  |> exit
