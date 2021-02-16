open Migrate_parsetree
open OCaml_403.Ast
open Parsetree

let img_regexp = Str.regexp "^ *@img \\([a-z.]*\\)"

let img_subst (s : string) =
  Str.global_replace img_regexp "{%html:<img alt=\"\\1\" src=\"img/\\1\"/>%}" s

let rewriter _config _cookies =
  let super = Ast_mapper.default_mapper in
  let attribute _self ((t, p) as a : attribute) =
    if t.txt = "ocaml.text" || t.txt = "ocaml.doc" then
      ( t,
        match p with
        | PStr
            [
              ( {
                  pstr_desc =
                    Pstr_eval
                      ( ( {
                            pexp_desc = Pexp_constant (Pconst_string (doc, None));
                            _;
                          } as pexp ),
                        [] );
                  _;
                } as pstr );
            ] ->
            PStr
              [
                {
                  pstr with
                  pstr_desc =
                    Pstr_eval
                      ( {
                          pexp with
                          pexp_desc =
                            Pexp_constant (Pconst_string (img_subst doc, None));
                        },
                        [] );
                };
              ]
        | _ -> failwith "not implemented kind of comment" )
    else a
  in
  { super with attribute }

let () = Driver.register ~name:"ppx1" (module OCaml_403) rewriter
