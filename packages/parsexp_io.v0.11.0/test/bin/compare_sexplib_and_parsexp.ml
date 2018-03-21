open Core

let try_withx f x = Result.try_with (fun () -> f x)

let tested = ref 0

module Cst_to_sexp_with_layout = struct
  module Src = Parsexp.Cst
  module Dst = Sexplib.Type_with_layout.Parsed

  let convert_pos ?(delta=0) (pos : Parsexp.Positions.pos) : Sexplib.Src_pos.Absolute.t =
    (* Sexplib.Type_with_layout columns are 1-based while parsexp ones are 0-based *)
    { col = pos.col + 1 + delta
    ; row = pos.line
    }

  let rec convert : Src.t -> Dst.t = function
    | Atom { loc; atom; unescaped } ->
      let atom =
        match unescaped with
        | None -> atom
        | Some unescaped ->
          (* We already test that the main sexplib parser and parsexp parse atoms the
             same way. The sexplib lexer parses atom differently and that's what is used
             for parsing sexps with layout.

             We assume that the Parsexp CST parser parses atoms the same way that the
             normal parsexp parser, and just use the sexplib lexer here. *)
          match Sexplib.Lexer.main (Lexing.from_string unescaped) with
          | STRING s -> s
          | _ -> assert false
      in
      Atom (convert_pos loc.start_pos, atom, unescaped)
    | List { loc; elements } ->
      List (convert_pos loc.start_pos,
            List.map elements ~f:convert_t_or_comment,
            (* Sexplib stores the position of the ')' while parsexp ranges are intervals
               with inclusive lower-bound and exclusive upper-bound. *)
            convert_pos loc.end_pos ~delta:(-1))

  and convert_t_or_comment : Src.t_or_comment -> Dst.t_or_comment = function
    | Sexp    t -> Sexp (convert t)
    | Comment c -> Comment (convert_comment c)

  and convert_comment : Src.comment -> Dst.comment = function
    | Plain_comment { loc; comment } ->
      Plain_comment (convert_pos loc.start_pos, comment)
    | Sexp_comment { hash_semi_pos; comments; sexp } ->
      Sexp_comment (convert_pos hash_semi_pos,
                    List.map comments ~f:convert_comment,
                    convert sexp)
end

let test_one_kind ~ascii ~what ~sexplib_load ~parsexp_load fn ~sexp_of:sexp_of_a =
  let s1 = sexplib_load fn in
  let s2 = parsexp_load fn in
  let fail () =
    printf "Differences in %s for %s!\n%!" fn what;
    Sexplib.Sexp.save_hum "sexplib.sexp.tmp" ([%sexp_of: (a list, exn) Result.t] s1);
    Sexplib.Sexp.save_hum "parsexp.sexp.tmp" ([%sexp_of: (a list, exn) Result.t] s2);
    let (_exit_code : int) =
      ksprintf
        Sys.command "patdiff %s sexplib.sexp.tmp parsexp.sexp.tmp"
        (if ascii then "-ascii" else "")
    in
    exit 1
  in
  match s1, s2 with
  | Ok s1, Ok s2 -> if s1 <> s2 then fail ()
  | Ok _, Error _ | Error _, Ok _ -> fail ()
  | Error _, Error _ -> ()

let test fn ~ascii =
  incr tested;
  test_one_kind ~ascii ~what:"the normal parser vs sexplib"
    ~sexplib_load:(try_withx Sexplib.Sexp.load_sexps)
    ~parsexp_load:(try_withx (fun filename ->
      Parsexp_io.load_exn (module Parsexp.Many) ~filename))
    ~sexp_of:Fn.id
    fn;
  test_one_kind ~ascii ~what:"the cst parser vs sexplib with layout"
    ~sexplib_load:(try_withx (fun filename ->
      In_channel.with_file filename ~f:(fun ic ->
        Sexplib.Sexp.With_layout.(Parser.sexps_abs Lexer.main) (Lexing.from_channel ic))))
    ~parsexp_load:(try_withx (fun filename ->
      Parsexp_io.load_exn (module Parsexp.Many_cst) ~filename
      |> List.map ~f:Cst_to_sexp_with_layout.convert_t_or_comment))
    ~sexp_of:[%sexp_of: Sexplib.Type_with_layout.Parsed.t_or_comment]
    fn;
  test_one_kind ~ascii ~what:"the normal parser vs the cst one"
    ~sexplib_load:(try_withx (fun filename ->
      Parsexp_io.load_exn (module Parsexp.Many) ~filename))
    ~parsexp_load:(try_withx (fun filename ->
      Parsexp_io.load_exn (module Parsexp.Many_cst) ~filename
      |> Parsexp.Cst.Forget.t_or_comments))
    ~sexp_of:Fn.id
    fn

let rec walk dir ~f =
  Array.iter (Sys.readdir dir) ~f:(fun fn ->
    let full_fn = dir ^/ fn in
    if Sys.is_directory full_fn = `Yes then
      walk full_fn ~f
    else if fn = "jbuild" || String.is_suffix fn ~suffix:".sexp" then
      f full_fn)

let () =
  Command.run
    (Command.basic ~summary:"compare Parsexp and Sexplib on the whole tree"
       (let open Command.Let_syntax in
        let%map_open
          ascii = flag "-ascii" no_arg ~doc:" disable colors"
        and
          root = flag "-root" (required string) ~doc:" repo root" in
        fun () ->
          walk root ~f:(test ~ascii);
          printf "tested: %d\n" !tested))
