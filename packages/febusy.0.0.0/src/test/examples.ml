include Nonstd
module String = Sosa.Native_string

let dbg fmt = ksprintf (eprintf "ExamplesDBG: %s\n%!") fmt

let ( // ) = Filename.concat

(* Example: return_date: Custom artifact

`return_date "identifier"` is an artifact
that gets the time of day but changes to it do not trigger rebuilds.
*)
let return_date (id : string) =
  let open Febusy.Edsl in
  let value = `Date (Unix.gettimeofday ()) in
  return
    (Custom
       { id
       ; to_string= (fun _ -> id)
       ; serialize= (fun _ _ -> id)
       ; deserialize_exn= (fun _ _ -> value)
       ; hash= (fun _ _ -> id)
       ; materialize= (fun _ -> Some value) })
    value

(* Example: usage_report: Run `find` and `du -sh` commands


Here is the signature:

```ocaml
val usage_report:
    ?depth:int -> string -> string ->
    (Febusy.Edsl.File.spec Febusy.Build.Artifact.custom, [ `File of string ])
    Febusy.Build.Artifact.t Febusy.Build.DAG.t
```

The value `let dag = usage_report ~depth:2 "/usr" "/tmp/ur-usr"` is a
build direct acyclic graph which produces an HTML file (`` `File path``) 
which contains a report of the size of all the folders found at depth
2 under the `/usr` directory.

To run it with the tests executable do:

    _build/default/src/test/main.exe usage-report --state /tmp/ur.state \
        --from /usr/ --to /tmp/ur-usr/

You should then be able to visit <file:///tmp/ur-usr/index.html>.

*)
let usage_report ?(depth = 2) path_from path_to =
  let default_css =
    "https://bootswatch.com/3/bower_components/"
    ^ "bootstrap/dist/css/bootstrap.min.css"
  in
  let open Febusy.Edsl in
  let list_of_paths =
    ksprintf System.cmd_to_string_list
      "find %s -maxdepth %d -mindepth %d -type d" path_from depth depth
  in
  let du_dash_sh dir =
    File.make (path_to // Digest.(string dir |> to_hex))
    @@ fun fp ->
    dbg "du_dash_sh: %S" dir ;
    System.cmdf "mkdir -p %s ; ( du -sh %s || echo 'Unknown: %s' ; ) > %s"
      path_to dir fp fp
  in
  let all_dus = join (List.map list_of_paths ~f:du_dash_sh) in
  all_dus =<>= return_date "report-date"
  >>= fun _ (dus, `Date report_date) ->
  string "report-markdown"
  <-- (fun () ->
        dbg "report with %d dus" (List.length dus) ;
        sprintf "Usage Report\n============\n\nDate: `%.02f`\n\n%s\n\n"
          report_date
          ( List.map dus ~f:(fun (`File f) ->
                let l = System.read_lines f |> String.concat ~sep:"\n" in
                sprintf "* `%s`\n" l )
          |> String.concat ~sep:"" ) )
  >>= fun _ report_md ->
  File.make (path_to // "report.md") (fun file_path ->
      System.cmdf "mkdir -p %s" Filename.(dirname file_path |> quote) ;
      System.write_lines file_path [report_md] )
  >>= fun _ (`File md_file) ->
  File.make (path_to // "index.html") (fun ih ->
      System.cmdf
        "pandoc -i %s -V title='Report' -V pagetitle='Report' -s --css %s -o %s"
        md_file default_css ih )

(* Example: random_one: Uninteresting more complex DAG

*)
let random_one () =
  let open Febusy.Edsl in
  let cmd_to_file path cmd =
    File.make ("/tmp/ro/" // path) (fun fp ->
        dbg "making %s" fp ;
        System.cmdf "mkdir -p /tmp/ro ; %s > %s" cmd fp )
  in
  let always_changes =
    string "changing-date"
    <-- fun () ->
    dbg "make date string" ;
    System.cmd_to_string_list "date" |> String.concat ~sep:"\n"
  in
  let oned5 =
    cmd_to_file "one.txt" "date -R"
    >>= fun _ (`File one) -> cmd_to_file "oned5.txt" (sprintf "md5sum %s" one)
  in
  let pair =
    string "pair-left" ** file "/tmp/ro/pair-right"
    <-- fun () ->
    dbg "Making the pair" ;
    System.write_lines "/tmp/ro/pair-right" ["one"] ;
    ("content", `File "/tmp/ro/pair-right")
  in
  let some_list =
    List.init 10 ~f:(fun i ->
        cmd_to_file (sprintf "item%d" i) (sprintf "echo %d" i) )
    |> join
  in
  let opt =
    always_changes
    >>= fun _ date ->
    let random = Digest.(string date |> to_hex) |> String.get_exn ~index:0 in
    dbg "random: %c" random ;
    return_value "opt-yes-no" @@ if random < '8' then Some date else None
  in
  let date_file =
    opt
    >>= fun _ date_opt ->
    match date_opt with
    | Some date ->
        File.make "/tmp/ro/daate.txt"
        @@ fun fp ->
        dbg "making file %s" fp ;
        System.write_lines fp [date]
    | None ->
        File.make "/tmp/ro/not-date.txt"
        @@ fun fp ->
        dbg "making file %s" fp ;
        System.write_lines fp ["NO-DATE"]
  in
  some_list =<>= pair =<>= oned5 =<>= date_file
  >>= fun _ (((the_list, (_, `File pright)), `File one), `File fdat) ->
  (* dbg "opt: %s" (Option.value opt ~default:"NO"); *)
  cmd_to_file "two.txt"
    (sprintf "cat %s %s %s %s" one fdat pright
       (List.map ~f:(fun (`File f) -> f) the_list |> String.concat ~sep:" "))

(* Example: tiny_example: Minimalistic `A→B→C` DAG

We build `src/lib/jbuild` out of `please.ml`, and in turn the library
`.cma` and `.cmxa`.
*)
let tiny_example () =
  let open Febusy.Edsl in
  File.return "please.ml"
  >>= fun _ pls ->
  (* The `>>=` operator induces a dependency. *)
  File.make "src/lib/jbuild" (fun _ -> System.cmdf "ocaml please.ml configure")
  >>= fun _ jbuild ->
  File.List.make
    [ (* This “target” is a multi-file one. *)
      "_build/default/src/lib/febusy.cma"
    ; "_build/default/src/lib/febusy.cmxa" ] (fun _ ->
      System.cmdf "jbuilder build @install" )

(* Example: build_website: Building the documentation website

The function `build_website` creates a *build-DAG* for a given `~output` path.
*)
let build_website ~output =
  let open Febusy.Edsl in
  let odoc_doc =
    let main_deps =
      (* We consider that these 2 files should trigger the re-build of
         the documentation in case their checksum changes. *)
      File.List.return ["_build/default/src/lib/febusy.cma"; "src/lib/edsl.mli"]
    in
    let making =
      [ "index.html"
      ; "odoc.css"
      ; "febusy/Febusy/Build/index.html"
      ; "febusy/Febusy/Edsl/index.html" ]
      |> List.map ~f:(fun p -> output // "api" // p)
    in
    main_deps
    >>= fun _ _ ->
    (* --> Introduction of the dependency on `main_deps`. *)
    File.List.make making (fun _ ->
        System.cmdf "jbuilder build @doc" ;
        System.cmdf "rm -fr %s/api ; mkdir -p %s/api/" output output ;
        System.cmdf "cp -r _build/default/_doc/_html/* %s/api/" output )
  in
  let css =
    (* We build the CSS file as the one of the API docs with some
       additional statements. *)
    let css_name =
      (* Given how HTML websites work, we need to keep the relative
         file path of the CSS around: *)
      "febusydoc.css"
    in
    let extra_css =
      (* A “named” value to introduce in the dependency graph: changes
         to the list will trigger rebuilds of the dependencies: *)
      return_value "febusy-extra-css"
        [ "body { width: 70em; max-width: 70em; font-size: 100% }"
        ; ".title { font-size: 200%; color: #007}"
        ; "h2 {color: #007}" ]
    in
    let odoc_part =
      (* We get the CSS from the API docs, hence we depend on them. *)
      odoc_doc >>= fun _ _ -> File.return (output // "api/odoc.css")
    in
    extra_css =<>= odoc_part
    (* ⤷ Use of the `=<>=` operator to join 2 dependency graphs.
       Results in a pair of computation results: *)
    >>= fun _ (extras, `File odoc_css) ->
    File.make (output // css_name) (fun p ->
        (* We use the contents of the CSS of the API-docs and then add
           the extra lines. *)
        System.cmdf "cat %s > %s" (Filename.quote odoc_css) (Filename.quote p) ;
        let o = open_out_gen [Pervasives.Open_append] 0o500 p in
        List.iter extras ~f:(fprintf o "\n%s\n") ;
        close_out o )
    >>= fun _ (`File css) ->
    (* Instead of the full path of the resulting CSS file, we pass
       around the relative path with a custom polymorphic variant to make
       sure there are no confusions. *)
    return_value "css" (`Css css_name)
  in
  let title =
    return_value "index-title"
      (`Title "Febusy: Flexible Embedded Build System")
  in
  let examples =
    let md =
      (* We transform the examples.ml file into a examples.md document: *)
      File.return "src/test/examples.ml"
      >>= fun _ (`File exs) ->
      let lines = System.read_lines exs in
      File.make (output // "examples.md") (fun mdf ->
          (* The idea is to detect OCaml comments which start with `"
             Example"` and add a “section-jump” there. *)
          let rec process_lines = function
            | [] -> []
            | one :: more when String.is_prefix one ~prefix:"(* Example" ->
                let comment, next =
                  List.split_while more ~f:(fun l -> l <> "*)")
                in
                let title =
                  match
                    String.split ~on:(`Character ':') one
                    |> List.map ~f:String.strip
                  with
                  | [_; tag; title] -> sprintf "### %s {#%s}" title tag
                  | other -> ksprintf failwith "Can't deal with line: %s" one
                in
                ["```"; ""; title; ""] @ comment @ [""; "```ocaml"]
                @ process_lines (List.tl_exn next)
            | other :: more -> other :: process_lines more
          in
          System.write_lines mdf
            ( ["### Preamble"; ""; "```ocaml"]
            @ process_lines lines @ ["```"; ""] ) )
    in
    md =<>= css
    (*        ⤷ we use the CSS created above. *)
    >>= fun _ (`File mdex, `Css css) ->
    File.make (output // "examples.html") (fun exhtml ->
        System.cmdf
          "mkdir -p %s ; pandoc -i %s -o %s -s --toc -V pagetitle=Examples -V \
           title=Examples --css %s"
          output mdex exhtml css )
  in
  let seds =
    (* “Seds” are a list of (A × B) tuples which will become `s/A/B/g`
       sed calls below.
       We inject them in the dependency graph so that their
       modification also re-triggers the build of the index page. *)
    let ocaml_mod ?v m =
      let mpath =
        sprintf "`%s`"
          (String.concat ~sep:"\\."
             (m @ Option.value_map ~default:[] v ~f:(fun v -> [v])))
      in
      ( mpath
      , sprintf "[%s](api\\/febusy\\/%s\\/index.html%s)" mpath
          (String.concat ~sep:"\\/" m)
          (Option.value_map v ~default:"" ~f:(sprintf "#val-%s")) )
    in
    return_value "the-seds"
      [ ("`src\\/test\\/examples\\.ml`", "[`examples.ml`](examples.html)")
      ; ("`let \\([a-z_]*\\) [^`]*`", "[\\0](examples.html#\\1)")
      ; ocaml_mod ["Febusy"; "Edsl"]
      ; ocaml_mod ["Febusy"; "Edsl"; "Make_unix"]
      ; ocaml_mod ["Febusy"; "Edsl"; "Make_unix"] ~v:"run" ]
  in
  let index =
    (* The index page is made from the README.md file and a
       few other dependencies: *)
    File.return "README.md" =<>= title =<>= css =<>= seds
    >>= fun _ (((`File readme_md, `Title title), `Css css), seds) ->
    File.make (output // "index.html") (fun fp ->
        dbg "Building %s" fp ;
        let sed_cmds =
          List.map seds ~f:(fun (rpl, wth) -> sprintf "sed 's/%s/%s/g'" rpl wth)
          |> String.concat ~sep:" | "
        in
        System.cmdf ~silent:false
          "mkdir -p %s ; tail -n +3 %s | %s | pandoc -V title='%s' -V \
           pagetitle='%s' -s --css %s --toc -o %s"
          output readme_md sed_cmds title title css fp )
  in
  (* Finally, the whole website is put together by depending on both
     the index page and the examples page. *)
  index =<>= examples
