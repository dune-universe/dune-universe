type t =
  {
  foo: int [@default 42][@ocaml.doc " prout "];
  bar: bool ;
  goo: string list [@sep ':']}[@@ocaml.doc " Coucou "][@@deriving cmdliner]
[@@xrefs [`Main]][@@envs [("PLOP", (Some "It rules!"), None)]][@@version
                                                                "4.2"]
include
  struct
    let foo_cmdliner_t =
      let infos =
        let docs = None in
        let doc = Some "prout" in
        let docv = None in
        let env = None in
        Cmdliner.Arg.info ?docs ?doc ?docv ?env ["f"; "foo"] in
      let open Cmdliner.Arg in value & ((opt int 42) & infos)
    let bar_cmdliner_t =
      let infos =
        let docs = None in
        let doc = None in
        let docv = None in
        let env = None in
        Cmdliner.Arg.info ?docs ?doc ?docv ?env ["b"; "bar"] in
      let open Cmdliner.Arg in value & (flag infos)
    let goo_cmdliner_t =
      let infos =
        let docs = None in
        let doc = None in
        let docv = None in
        let env = None in
        Cmdliner.Arg.info ?docs ?doc ?docv ?env ["g"; "goo"] in
      let open Cmdliner.Arg in
        let open Cmdliner.Term in
          (const List.concat) $
            (value & ((opt_all (list ?sep:(Some ':') string) []) & infos))
    let cmdliner_t =
      let mk foo bar goo = { foo; bar; goo } in
      let open Cmdliner.Term in
        (((const mk) $ foo_cmdliner_t) $ bar_cmdliner_t) $ goo_cmdliner_t
    let cmdliner f =
      let name = Filename.basename Sys.executable_name in
      let open Cmdliner in
        let info =
          Term.info name ?man_xrefs:(Some [`Main]) ?man:None
            ?envs:(Some
                     [Term.env_info ?docs:None ?doc:(Some "It rules!") "PLOP"])
            ?doc:(Some "Coucou") ?version:(Some "4.2")
            ~exits:Term.default_exits in
        let term_t = let open Term in (const f) $ cmdliner_t in
        Term.exit @@ (Term.eval (term_t, info))
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let f t = t.foo + 1
let _ = cmdliner f
