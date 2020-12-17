open Fmlib
open Alba_core

module Command_parser =
    Parser_lang.Make (Parser_lang.Command)

module Located = Character_parser.Located


module Make (Io: Io.SIG) =
struct

    module Cli_state =
    struct
        type entry = {
            input: string;
            context_loaded: Context.t;
            context: Context.t;
            loaded_file: string option
        }

        type t = entry option

        let init: t =
            let context = Standard_context.make () in
            Some {
                input = "";
                context_loaded = context;
                context;
                loaded_file = None;
            }


        let clear: t -> t =
            Option.map
                (fun entry -> {entry with context = entry.context_loaded})


        let exit: t = None


        let prompt (state: t): string option =
            Option.map
                (fun entry ->
                    if entry.input = "" then
                        "> "
                    else
                        "| ")
                state


        let add (line: string) (state: t): t =
            Option.map
                (fun entry ->
                    {entry with
                        input =
                            if entry.input = "" then
                                line
                            else
                                entry.input ^ "\n" ^ line
                    }
                )
                state


        let put_context (context: Context.t) (state: t): t =
            Option.map
                (fun entry ->
                    {entry with context; input = ""})
                state

        let put_loaded (name: string) (context: Context.t): t -> t =
            Option.map
                (fun _ ->
                    {
                        input = "";
                        context;
                        context_loaded = context;
                        loaded_file = Some name;
                    }
                )


        let clear_input (state: t): t =
            Option.map
                (fun entry ->
                    {entry with input = ""})
                state

        let entry (state: t): entry =
            match state with
            | None ->
                assert false (* Illegal call! *)
            | Some entry ->
                entry

        let input (state: t): string =
            (entry state).input

        let context (state: t): Context.t =
            (entry state).context
    end (* Cli_state *)




    module Pretty =
    struct
        module Out =
            Fmlib.Io.Output (Io)

        include
            Pretty_printer.Pretty (Out)

        let run (pr: t): unit Io.t =
            Out.run
                Io.File.stdout
                (run 0 80 80 pr)
    end (* Pretty *)




    let parse (input: string): Command_parser.parser =
        let len = String.length input
        in
        let module P = Command_parser in
        let rec parse i p =
            let more = P.needs_more p
            in
            if i < len && more then
                parse (i + 1) (P.put_character p input.[i])
            else if more then
                P.put_end p
            else
                p
        in
        parse 0 P.(make command)


    let build_and_compute
        (input: string)
        (context: Context.t)
        (expression: Ast.Expression.t)
        (compute: bool)
        : Pretty.t
        =
        match
            Build_expression.build expression context
        with
        | Error problem ->
            let module Builder_print = Build_problem.Print (Pretty) in
            Builder_print.print_with_source input problem

        | Ok (term, typ) ->
            let term =
                let term =
                    if compute then
                        Context.compute term context
                    else
                        term
                in
                match term with
                | Term.Typed (term, _) ->
                    term
                | _ ->
                    term
            in
            let open Pretty in
            let module P = Context.Pretty (Pretty)
            in
            cut
            <+>
            P.print Term.(Typed (term, typ)) context
            <+>
            cut


    let add_definition
        (def: Ast.Expression.definition)
        (state: Cli_state.t)
        : Cli_state.t Io.t
    =
        let open Io in
        match
            Builder.add_definition def (Cli_state.context state)
        with
        | Ok context ->
            return (Cli_state.put_context context state)
        | Error problem ->
            let module Builder_print = Build_problem.Print (Pretty) in
            Pretty.run
                (Builder_print.print_with_source
                    (Cli_state.input state)
                    problem)
            >>= fun _ ->
            return (Cli_state.clear_input state)



    let process_input (state: Cli_state.t): Cli_state.t Io.t =
        let continue_after action =
            Io.(
                action
                >>= fun _ ->
                return (Cli_state.clear_input state))
        in
        let input = Cli_state.input state
        in
        let p = parse input
        in
        assert (Command_parser.has_ended p);
        match Command_parser.result p with
        | Some cmd ->
            assert (Command_parser.has_succeeded p);
            (
                match cmd with
                | Parser_lang.Command.Do_nothing ->
                    Io.return (Cli_state.clear_input state)

                | Parser_lang.Command.Exit ->
                    Io.return Cli_state.exit

                | Parser_lang.Command.Evaluate expression ->
                    continue_after
                        (Pretty.run
                            (build_and_compute
                                input
                                (Cli_state.context state)
                                expression
                                true))

                | Parser_lang.Command.Type_check expression ->
                    continue_after
                        (Pretty.run
                            (build_and_compute
                                input
                                (Cli_state.context state)
                                expression
                                false))

                | Parser_lang.Command.Clear ->
                    Io.return (Cli_state.clear state)

                | Parser_lang.Command.Load file ->
                    let module Compile = Module.Make (Io) in
                    Io.(
                        Compile.compile file >>= function
                        | None ->
                            Io.return (Cli_state.clear_input state)
                        | Some context ->
                            Io.return
                                (Cli_state.put_loaded
                                    (Located.value file)
                                    context
                                    state)
                    )

                | Parser_lang.Command.Reload ->
                    assert false (* nyi *)

                | Parser_lang.Command.Define def ->
                    add_definition def state
            )
        | None ->
            let module Printer =
                Command_parser.Error_printer (Pretty)
            in
            continue_after
                (Pretty.run
                    (Printer.print_with_source input p))



    let next (state: Cli_state.t) (line: string): Cli_state.t Io.t =
        assert (Cli_state.prompt state <> None); (* guaranteed by [cli_loop]. *)
        let state = Cli_state.add line state
        in
        let is_last (line: string): bool =
            let len = String.length line in
            len = 0
            || line.[len - 1] <> ' '
        in
        if is_last line then
            process_input state
        else
            Io.return state


    let stop (state: Cli_state.t): Cli_state.t Io.t =
        Io.return state


    let run_cli _: unit Io.t =
        Io.(
            cli_loop
                Cli_state.init
                Cli_state.prompt
                next
                stop
            >>= fun _ ->
            return ()
        )


    module Evaluate_stdin =
    struct
        module Expression_parser =
            Parser_lang.Make (Ast.Expression)


        module Writable =
        struct
            type t = {
                can_end: bool;
                input: string;
                parser: Expression_parser.parser;
            }

            let init (): t =
                {
                    can_end =
                        false;

                    input =
                        "";

                    parser =
                        Expression_parser.(make (expression ()));
                }

            let needs_more (w: t): bool =
                Expression_parser.needs_more w.parser
                ||
                not w.can_end


            let put_character (w: t) (c: char): t =
                let open Expression_parser in
                {
                    can_end =
                        c = '\n';

                    input =
                        w.input ^ Common.String.one c;

                    parser =
                        if needs_more w.parser then
                            put_character w.parser c
                        else
                            w.parser;
                }

            let put_end (w: t): t =
                let open Expression_parser in
                { w with
                    parser =
                        if needs_more w.parser then
                            put_end w.parser
                        else
                            w.parser;
                }

            let result (w: t): string * Expression_parser.parser =
                w.input,
                w.parser
        end (* Writable *)



        let run _: unit Io.t =
            let module R = Io.File.Read (Writable) in
            let module Error = Fmlib.Io.Error in
            Io.(
                R.read File.stdin (Writable.init ())
                >>= fun io_res ->
                match io_res with
                | Error (_, error) ->
                    Pretty.(run
                        (string (Error.message error) <+> cut)
                    )
                | Ok w ->
                    let input, p = Writable.result w in
                    let open Expression_parser
                    in
                    assert (has_ended p);
                    match result p with
                    | None ->
                        let module Printer =
                            Error_printer (Pretty)
                        in
                        Pretty.run
                            (Printer.print_with_source input p)

                    | Some expression ->
                        Pretty.run
                            (build_and_compute
                                input
                                (Standard_context.make ())
                                expression
                                false)
            )
    end (* Evaluate_stdin *)



    let run_eval _: unit Io.t =
        Evaluate_stdin.run ()
end
