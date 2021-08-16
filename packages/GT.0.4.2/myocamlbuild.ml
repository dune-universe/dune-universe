open Printf;;
open Ocamlbuild_plugin;;

let m4_rules ext =
  let dep   = "%(name)" -.- "ml4"
  and prod1 = "%(name)" -.- "ml"
  and prod2 = "%(name: <**/*>)" -.- "ml" in
  let cppo_rule prod env _build =
    let dep = env dep in
    let prod = env prod in
    (* let tags = tags_of_pathname prod ++ "cppo" in *)
    Cmd (S[A "m4"; A"../src/macro.m4"; P dep; Sh ">"; A prod ])
  in
  rule ("cppo: *.ml4 -> *.ml")       ~dep ~prod:prod1 (cppo_rule prod1);
  rule ("cppo: **/*.ml4 -> **/*.ml") ~dep ~prod:prod2 (cppo_rule prod2)

open Command;;

let make_plugins_args ~is_byte =
  (* N.B. Order matters *)
  let names =
    [ "show"; "gmap"
    ; "eval";  "compare"
    ; "eq"
    ;  "stateful"; "foldl"; "foldr"
(*    ; "show_typed" *)
    ; "gfmt"
    ; "html"
    ; "hash"
(*    ; "htmlTy" *)
    ]
  in
  List.map (fun s -> A(Printf.sprintf "plugins/%s.cm%s" s (if is_byte then "o" else "x")) )
    names

let () = dispatch (fun hook ->
  match hook with
  | Before_rules -> ()
  | After_rules ->
    ocaml_lib "common/GTCommon";
    ocaml_lib "mymetaquot/mymetaquot";
    ocaml_lib "src/GTlib";
    (* flag ["compile"; "short_paths"] & S [A "-short-paths"]; *)

    flag ["compile"; "native"; "use_GT"]   (S [ A"-I";A"src" ]);
    flag ["compile"; "byte";   "use_GT"]   (S [ A"-I";A"src" ]);
    flag ["link";    "byte";   "use_GT"]   (S [ A"-I";A"src"; A"GTlib.cma" ]);
    flag ["link";    "native"; "use_GT"]   (S [ A"-I";A"src"; A"GTlib.cmxa" ]);

    flag ["compile"; "use_ppx_import"]
      (S [ A"-ppx"; A"`ocamlfind query ppx_import`/ppx_import --as-ppx"
         ; A"-ppx"; A"ppx/pp_gt.native --as-ppx"
         ]);
    flag ["ocamldep"; "use_ppx_import"]
      (S [ A"-ppx"; A"`ocamlfind query ppx_import`/ppx_import --as-ppx"
         ; A"-ppx"; A"ppx/pp_gt.native --as-ppx"
         ]);

    flag ["link";    "native"; "use_mymetaquot"]   (S [A"-linkall"
      (* BEWARE of the following line. It can make your PPX non-loadable *)
      (* ; A"-package"; A"ppxlib.runner" *)
      ]);

    m4_rules ();
    dep ["use_m4"] ["src/macro.m4"];
    flag ["ocaml"; "pp"; "use_pa_gt"] (S [ Sh"../camlp5o_pp.sh" ]);
    flag ["ocaml"; "link"; "link_pagtcmo"] (S [ A"camlp5/pa_gt.cma" ]);

    flag ["ocaml"; "link"; "link_pp5gt"]
      (S[ A"-package"; A"ppxlib"
        ; A"common/GTCommon.cma"
        ; A"camlp5/pa_gt.cma"
        ]);
    (* dep ["ocaml"; "link"; "link_pp5gt"]        ["common/GTCommon.cma"]; *)

     (* flag ["ocamldep"; "link_pa_gt"]   (S [ Sh"../camlp5o_pp.sh" ]);
      * flag ["compile";  "link_pa_gt"]   (S [ Sh"../camlp5o_pp.sh" ]); *)

    (* flag ["make_pp_mymetaquot"; "link"; "native"] @@
     * S ([ A"mymetaquot/lifters.cmx"; A"mymetaquot/my_metaquot.cmx"
     *
     *    ]); *)
    dep ["mymetaquot/pp_mymetaquot.native"]        ["mymetaquot/mymetaquot.cmxa"];

    flag ["make_pp_gt"; "link"; "byte"] @@
    S ([ A"ppx/ppx_deriving_gt.cma"; A"-package"; A"ppxlib" ] @
       make_plugins_args ~is_byte:true @ [ A"common/plugin.cmo"]  );
    flag ["make_pp_gt"; "link"; "native"] @@
    S ([ A"-package"; A"ppxlib"
       ; A"-I"; A"common"
       ; A"ppx/ppx_deriving_gt.cmxa"
       ] @
       make_plugins_args ~is_byte:false @
       [
       ]  );

    dep ["compile"; "use_ppx_extension"] ["ppx/ppx_deriving_gt.cma"; "rewriter/pp_gt.native"];
    dep ["compile"; "link_pp5gt"]        ["camlp5/core2.ml"];
    ()
 | _ -> ()
)
