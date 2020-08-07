open Fmlib
open Common
open Alba_core


module Parser = Parser_lang.Make (Unit)
module Located = Character_parser.Located



module Compiler =
struct
    type lines =
        string Sequence.t

    type error =
        | Parse_error
        | Build_error of Build_problem.t

    type value =
        Term.t * Term.typ * Context.t

    type t = {
        lines: lines;
        line:  string;
        terminate_line: bool; (* error occurred, but line not yet completed. *)
        parser: Parser.parser;
        context: Context.t;
        values: value list;
        error:  error option;
    }


    let make_from_context (context: Context.t): t =
        {
            lines = Sequence.empty;

            line  = "";

            terminate_line = false;

            parser = Parser.(make (source_file true));

            context;

            values = [];

            error = None;
        }


    let make _: t =
        make_from_context (Standard_context.make ())


    let context (c: t): Context.t =
        c.context


    let needs_more (compiler: t): bool =
        (
            compiler.error = None
            &&
            Parser.needs_more compiler.parser
        )
        ||
        compiler.terminate_line


    let has_succeeded (compiler: t): bool =
        Parser.has_succeeded compiler.parser
        &&
        compiler.error = None



    let make_semantic
        (parser: Parser.parser) (compiler: t)
        : error option * value list * Context.t
        =
        let open Parser_lang in
        let open Source_file in
        match top (Parser.state parser) with
        | Expression (eval_flag, expression) ->
        (
            match Build_expression.build expression compiler.context with
            | Error problem ->
                Some (Build_error problem),
                compiler.values,
                compiler.context

            | Ok (term, typ) ->
                let term, typ =
                    match term with
                    | Term.Typed (term, typ) ->
                        term, typ
                    | _ ->
                        term, typ
                in
                let term =
                    if eval_flag then
                        Context.compute term compiler.context
                    else
                        term
                in
                None,
                (term, typ, compiler.context) :: compiler.values,
                compiler.context
        )
        | Entry entry ->
        (
            match Builder.add_entry entry compiler.context with
            | Ok context ->
                None, compiler.values, context

            | Error problem ->
                Some (Build_error problem),
                compiler.values,
                compiler.context
        )


    let add_character (c: char) (compiler: t) : string * lines =
        if c = '\n' then
            "",
            Sequence.push compiler.line compiler.lines
        else
            compiler.line ^ String.one c,
            compiler.lines


    let make_step (c: char) (parser: Parser.parser) (compiler: t): t =
        (* The parser has already made its step, now we do the semantics. *)
        let failed = Parser.has_failed parser
        in
        let error, values, context =
            if failed then
                Some Parse_error, compiler.values, compiler.context
            else
                let src0 = Parser.state compiler.parser
                and src1 = Parser.state parser
                in
                if
                    Parser_lang.Source_file.(count src0 < count src1)
                then
                    make_semantic parser compiler
                else
                    None, compiler.values, compiler.context
        and line, lines =
            add_character c compiler
        in
        {
            line;

            lines;

            terminate_line = failed (*&& c <> '\n'*);

            parser;

            context;

            error;

            values;
        }


    let put_character (compiler: t) (c: char): t =
        assert (needs_more compiler);
        if compiler.terminate_line then
            let line, lines = add_character c compiler in
            { compiler with
                line;

                lines;

                terminate_line =
                    c <> '\n';
            }
        else
            let parser =
                Parser.put_character compiler.parser c
            in
            make_step c parser compiler



    let put_end (compiler: t): t =
        assert (needs_more compiler);
        if compiler.terminate_line then
            let line, lines = add_character '\n' compiler in
            { compiler with
                line;
                lines;
                terminate_line = false;
            }
        else
            let parser = Parser.put_end compiler.parser in
            make_step '\n' parser compiler



    module Print (Pretty: Pretty_printer.SIG) =
    struct
        let print_error (compiler: t): Pretty.t =
            let lines =
                Sequence.push compiler.line compiler.lines
            in
            match compiler.error with
            | None ->
                assert false (* Illegal call! *)
            | Some Parse_error ->
                let module Parser_print =
                    Parser.Error_printer (Pretty)
                in
                Parser_print.print_with_source_lines
                    lines
                    compiler.parser
            | Some (Build_error problem) ->
                let module Builder_print =
                    Build_problem.Print (Pretty)
                in
                Builder_print.print_with_source_lines
                    lines
                    problem

        let print_values (compiler: t): Pretty.t =
            let module Context_print = Context.Pretty (Pretty) in
            Pretty.(chain
                (List.rev_map
                    (fun (term, typ, context) ->
                        Context_print.print
                            Term.(Typed (term, typ))
                            context
                        <+>
                        cut)
                    compiler.values)
            )
    end
end





module Make (Io: Io.SIG) =
struct
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



    let compile (file: string Located.t): Context.t option Io.t =
        let name = Located.value file
        in
        let open Io in
        File.In.open_ name
        >>= function
        | Error error ->
            Pretty.(run
                (
                    string "Cannot open file '" <+> string name <+> char '\''
                    <+> cut <+> cut
                    <+> string (Fmlib.Io.Error.message error)
                    <+> cut <+> cut
                )
            )
            >>= fun _ ->
            return None

        | Ok fd ->
            let module Reader = File.Read (Compiler) in
            Reader.read fd (Compiler.make ())
            >>= fun io_result ->
            File.In.close fd
            >>= fun _ ->
            match io_result with
            | Error (_, error) ->
                let module Io_error = Fmlib.Io.Error in
                    Pretty.(run (string (Io_error.message error) <+> cut))
                    >>= fun _ ->
                    return None

            | Ok compiler ->
                let module Print = Compiler.Print (Pretty) in
                if Compiler.has_succeeded compiler then
                    return (Some (Compiler.context compiler))
                else
                    Pretty.run (Print.print_error compiler)
                    >>= fun _ -> return None


    let run _: unit Io.t =
        let module Reader = Io.File.Read (Compiler) in
        let open Io in
        Reader.read File.stdin (Compiler.make ())
        >>= fun io_result ->
        match io_result with
        | Error (_, error) ->
            let module Io_error = Fmlib.Io.Error in
            Io.(
                Pretty.(run (string (Io_error.message error) <+> cut))
                >>= fun _ ->
                exit 1)
        | Ok compiler ->
            let open Io in
            let module Print = Compiler.Print (Pretty) in
            Pretty.run (Print.print_values compiler)
            >>= fun _ ->
            if Compiler.has_succeeded compiler then
                return ()
            else
                Pretty.run (Print.print_error compiler)
end
