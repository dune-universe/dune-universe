(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
open OUnit2

(* cheker utils *)
let assert_equals_abs_form_by_erl_file expected_ast_res beam_filename test_ctx =
  let open Obeam in
  let beam_buf = Bitstring.bitstring_of_file beam_filename in
  match Beam.parse_layout beam_buf with
  | Ok (layout, _) ->
     let {
       Beam.cl_abst = opt_abst;
       Beam.cl_dbgi = opt_dbgi;
     } = layout in
     let parsed_result =
       match opt_abst with
       | Some abst ->
          External_term_format.parse abst.Beam.abst_buf
       | None ->
          begin
            match opt_dbgi with
            | Some dbgi ->
               External_term_format.parse dbgi.Beam.dbgi_buf
            | None ->
               failwith "abst and dbgi chunk is not found"
          end
     in
     let _ =
       match parsed_result with
       | Ok (etf, _rest) ->
          let actual_ast_res = Abstract_format.of_etf etf in
          assert_equal expected_ast_res actual_ast_res
                       ?printer:(Some (fun a ->
                                     a |> Result.sexp_of_t Abstract_format.sexp_of_t Abstract_format.sexp_of_err_t
                                     |> Sexp.to_string_hum
                                ))
       | Error (msg, _rest) ->
          failwith (Printf.sprintf "Failed: %s" msg)
     in
     ()
  | Error (msg, _rest) ->
     failwith (Printf.sprintf "Failed : %s\n" msg)

let assert_equals_abs_form_f f beam_filename test_ctx =
  let expected_ast_res = f () in
  assert_equals_abs_form_by_erl_file expected_ast_res beam_filename test_ctx

(* test cases *)
let template_01 () =
  let module Ast = Obeam.Abstract_format in
  Ast.AbstractCode
    (Ast.ModDecl
       [(Ast.AttrFile (1, "test01.erl", 1)); (Ast.AttrMod (1, "test01"));
        (Ast.AttrExport (3, [("g", 0); ("h", 1)]));
        (Ast.DeclFun (5, "f", 0,
                      [(Ast.ClsFun (5, [], None,
                                    (Ast.ExprBody
                                       [(Ast.ExprLit (Ast.LitInteger (6, 0)))])))
                      ]
                     ));
        (Ast.DeclFun (8, "g", 0,
                      [(Ast.ClsFun (8, [], None,
                                    (Ast.ExprBody
                                       [(Ast.ExprLit (Ast.LitInteger (9, 10)))])))
                      ]
                     ));
        (Ast.SpecFun (11, None, "h", 1,
                      [(Ast.TyFun
                          (11,
                           (Ast.TyProduct (11, [(Ast.TyPredef (11, "integer", []))])),
                           (Ast.TyPredef (11, "string", []))))
                      ]
                     ));
        (Ast.DeclFun (12, "h", 1,
                      [(Ast.ClsFun (12, [(Ast.PatUniversal 12)], None,
                                    (Ast.ExprBody
                                       [(Ast.ExprLit
                                           (Ast.LitString (13, "abcdefg")))
                                       ])
                                   ))
                      ]
                     ));
        Ast.FormEof])
  |> Result.return

let template_02 () =
  let module Ast = Obeam.Abstract_format in
  Ast.AbstractCode
    (Ast.ModDecl
       [(Ast.AttrFile (1, "test02.erl", 1)); (Ast.AttrMod (1, "test02"));
        (Ast.SpecFun (3, None, "i", 1,
                      [(Ast.TyContFun
                          (3,
                           (Ast.TyPredef (3, "fun",
                                          [(Ast.TyProduct (3, [(Ast.TyVar (3, "A"))]));
                                           (Ast.TyPredef (3, "integer", []))]
                                         )),
                           (Ast.TyCont
                              [(Ast.TyContRel
                                  (3, (Ast.TyContIsSubType 3),
                                   (Ast.TyVar (3, "A")),
                                   (Ast.TyPredef (3, "integer", []))
                                  ))
                              ])
                          ))
                      ]
                     ));
        (Ast.DeclFun (4, "i", 1,
                      [(Ast.ClsFun (4, [(Ast.PatVar (4, "N"))], None,
                                    (Ast.ExprBody [(Ast.ExprVar (4, "N"))])))
                      ]
                     ));
        (Ast.SpecFun (6, None, "j", 1,
                      [(Ast.TyContFun
                          (6,
                           (Ast.TyPredef (6, "fun",
                                          [(Ast.TyProduct (6, [(Ast.TyVar (6, "A"))]));
                                           (Ast.TyVar (6, "B"))]
                                         )),
                           (Ast.TyCont
                              [(Ast.TyContRel
                                  (6, (Ast.TyContIsSubType 6),
                                   (Ast.TyVar (6, "A")),
                                   (Ast.TyPredef (6, "integer", []))
                                  ));
                               (Ast.TyContRel
                                  (7, (Ast.TyContIsSubType 7),
                                   (Ast.TyVar (7, "B")),
                                   (Ast.TyPredef (7, "integer", []))))
                              ])
                          ))
                      ]
                     ));
        (Ast.DeclFun (8, "j", 1,
                      [(Ast.ClsFun
                          (8, [(Ast.PatVar (8, "N"))], None,
                           (Ast.ExprBody
                              [(Ast.ExprBinOp (9, "*", (Ast.ExprVar (9, "N")),
                                               (Ast.ExprVar (9, "N"))))
                              ])
                          ))
                      ]
                     ));
        Ast.FormEof])
  |> Result.return

let template_03 () =
  let module Ast = Obeam.Abstract_format in
  Ast.AbstractCode
    (Ast.ModDecl
       [(Ast.AttrFile (1, "test03.erl", 1)); (Ast.AttrMod (1, "test03"));
        (Ast.DeclFun (3, "f", 1,
                      [(Ast.ClsFun
                          (3, [(Ast.PatVar (3, "N"))],
                           (Some (Ast.GuardSeq
                                    [(Ast.Guard
                                        [(Ast.GuardTestCall
                                            (3,
                                             (Ast.LitAtom (3, "is_integer")),
                                             [(Ast.GuardTestVar (3, "N"))]))
                                        ])
                                    ])),
                           (Ast.ExprBody [(Ast.ExprLit (Ast.LitInteger (4, 0)))])));
                       (Ast.ClsFun
                          (5, [(Ast.PatUniversal 5)],
                           None,
                           (Ast.ExprBody [(Ast.ExprLit (Ast.LitInteger (6, 2)))])))
                      ]
                     ));
        (Ast.DeclFun (8, "g", 1,
                      [(Ast.ClsFun
                          (8, [(Ast.PatVar (8, "R"))],
                           None,
                           (Ast.ExprBody
                              [(Ast.ExprCase
                                  (9,
                                   (Ast.ExprVar (9, "R")),
                                   [(Ast.ClsCase
                                       (10,
                                        (Ast.PatLit
                                           (Ast.LitAtom (10, "ok"))),
                                        None,
                                        (Ast.ExprBody
                                           [(Ast.ExprLit
                                               (Ast.LitInteger (10, 1)))
                                        ])
                                    ));
                                    (Ast.ClsCase
                                       (11,
                                        (Ast.PatLit
                                           (Ast.LitAtom (11, "error"))),
                                        None,
                                        (Ast.ExprBody
                                           [(Ast.ExprLit
                                               (Ast.LitInteger (11, 2)))
                                        ])
                                    ))
                                   ]
                               ))
                           ])
                       ))
                      ]
        ));
        Ast.FormEof])
  |> Result.return

let template_04 () =
  let module Ast = Obeam.Abstract_format in
  Ast.AbstractCode
  (Ast.ModDecl
     [(Ast.AttrFile (1, "test04.erl", 1));
       (Ast.AttrMod (1, "test04"));
       (Ast.AttrExport (3, [("f", 0); ("g", 1); ("h", 1)]));
       (Ast.AttrExportType (4, [("tuple", 2); ("int", 0)]));
       (Ast.DeclType (6, "tuple", [(6, "A"); (6, "B")],
          (Ast.TyPredef (6, "tuple",
             [(Ast.TyVar (6, "A"));
               (Ast.TyVar (6, "B"))]
             ))
          ));
       (Ast.DeclOpaqueType (7, "int", [],
          (Ast.TyPredef (7, "integer", []))));
       (Ast.DeclFun (9, "f", 0,
          [(Ast.ClsFun (9, [], None,
              (Ast.ExprBody
                 [(Ast.ExprMapUpdate (9,
                     (Ast.ExprMapCreation (9,
                        [(Ast.ExprAssoc (9,
                            (Ast.ExprLit
                               (Ast.LitAtom (9, "a"))),
                            (Ast.ExprLit
                               (Ast.LitInteger (9, 1)))
                            ));
                          (Ast.ExprAssoc (9,
                             (Ast.ExprLit
                                (Ast.LitAtom (9, "b"))),
                             (Ast.ExprLit
                                (Ast.LitInteger (9, 2)))
                             ))
                          ]
                        )),
                     [(Ast.ExprAssocExact (9,
                         (Ast.ExprLit
                            (Ast.LitAtom (9, "a"))),
                         (Ast.ExprLit
                            (Ast.LitInteger (9, 42)))
                         ));
                       (Ast.ExprAssoc (9,
                          (Ast.ExprLit
                             (Ast.LitAtom (9, "c"))),
                          (Ast.ExprLit
                             (Ast.LitInteger (9, 3)))
                          ))
                       ]
                     ))
                   ])
              ))
            ]
          ));
       (Ast.DeclFun (11, "g", 1,
          [(Ast.ClsFun (11,
              [(Ast.PatMap (11,
                  [(Ast.PatAssocExact (11,
                      (Ast.PatLit
                         (Ast.LitAtom (11, "a"))),
                      (Ast.PatVar (11, "N"))))
                    ]
                  ))
                ],
              None,
              (Ast.ExprBody [(Ast.ExprVar (11, "N"))])
              ))
            ]
          ));
       (Ast.DeclFun (13, "h", 1,
          [(Ast.ClsFun (13, [(Ast.PatVar (13, "M"))],
              (Some (Ast.GuardSeq
                       [(Ast.Guard
                           [(Ast.GuardTestBinOp (13, "andalso",
                               (Ast.GuardTestBinOp (13, "=:=",
                                  (Ast.GuardTestVar (13, "M")),
                                  (Ast.GuardTestMapCreation (13,
                                     [(Ast.GuardTestAssoc (13,
                                         (Ast.GuardTestLit
                                            (Ast.LitAtom (13, "a"
                                               ))),
                                         (Ast.GuardTestLit
                                            (Ast.LitInteger (13,
                                               42)))
                                         ))
                                       ]
                                     ))
                                  )),
                               (Ast.GuardTestBinOp (13, "=:=",
                                  (Ast.GuardTestMapUpdate (13,
                                     (Ast.GuardTestVar (13, "M")),
                                     [(Ast.GuardTestAssocExact (
                                         13,
                                         (Ast.GuardTestLit
                                            (Ast.LitAtom (13, "a"
                                               ))),
                                         (Ast.GuardTestLit
                                            (Ast.LitInteger (13,
                                               0)))
                                         ))
                                       ]
                                     )),
                                  (Ast.GuardTestMapCreation (13,
                                     [(Ast.GuardTestAssoc (13,
                                         (Ast.GuardTestLit
                                            (Ast.LitAtom (13, "a"
                                               ))),
                                         (Ast.GuardTestLit
                                            (Ast.LitInteger (13,
                                               0)))
                                         ))
                                       ]
                                     ))
                                  ))
                               ))
                             ])
                         ])),
              (Ast.ExprBody [(Ast.ExprVar (13, "M"))])
              ))
            ]
          ));
       Ast.FormEof])
  |> Result.return

let template_05 () =
  let module Ast = Obeam.Abstract_format in
  Ast.AbstractCode
    (Ast.ModDecl
       [(Ast.AttrFile (1, "test05.erl", 1)); (Ast.AttrMod (1, "test05"));
        (Ast.DeclRecord (3, [(3, "a", None, None);
                             (4, "b", Some (Ast.ExprLit (Ast.LitInteger (4, 42))), None);
                             (5, "c", None, Some (Ast.TyPredef (5, "string", [])));
                             (6, "d", Some (Ast.ExprLit (Ast.LitInteger (6, 57))), Some (Ast.TyPredef (6, "integer", [])))]));
        Ast.FormEof])
  |> Result.return

let template_06 () =
  let module Ast = Obeam.Abstract_format in
  let module Sf = Obeam.Simple_term_format in
  Ast.AbstractCode
    (Ast.ModDecl
       [(Ast.AttrFile (1, "test06.erl", 1));
        (Ast.AttrMod (1, "test06"));
        (Ast.AttrWild (3, "compile",
                       (Sf.List [(Sf.Atom "export_all")])
        ));
        (Ast.AttrWild (5, "vsn",
                       (Sf.Atom "1.0.0")));
        (Ast.AttrWild (7, "on_load",
                       (Sf.Tuple (2,
                                  [(Sf.Atom "f"); (Sf.Integer 0)]
                       ))
        ));
        (Ast.AttrWild (9, "behaviour",
                       (Sf.Atom "gen_server")));
        (Ast.AttrWild (11, "foo",
                       (Sf.Binary "bar")));
        (Ast.DeclFun (13, "f", 0,
                      [(Ast.ClsFun (13, [], None,
                                    (Ast.ExprBody
                                       [(Ast.ExprLit
                                           (Ast.LitAtom (13, "ok")))
                                    ])
                       ))
                      ]
        ));
        Ast.FormEof])
  |> Result.return

let template_07 () =
  let module Ast = Obeam.Abstract_format in
  Ast.AbstractCode
    (Ast.ModDecl
     [(Ast.AttrFile (1, "test07.erl", 1));
       (Ast.AttrMod (1, "test07"));
       (Ast.AttrExport (3, [("s", 0); ("l", 0)]));
       (Ast.DeclFun (5, "s", 0,
          [(Ast.ClsFun (5, [], None,
              (Ast.ExprBody
                 [(Ast.ExprLit
                     (Ast.LitBigInt (6,
                        Z.of_string "123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
                        )))
                   ])
              ))
            ]
          ));
       (Ast.DeclFun (8, "l", 0,
          [(Ast.ClsFun (8, [], None,
              (Ast.ExprBody
                 [(Ast.ExprLit
                     (Ast.LitBigInt (9,
                        Z.of_string "123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
                        )))
                   ])
              ))
            ]
          ));
       Ast.FormEof])
  |> Result.return

let template_08 () =
  let module Ast = Obeam.Abstract_format in
   Ast.AbstractCode
     (Ast.ModDecl
      [(Ast.AttrFile (1, "test08.erl", 1));
        (Ast.AttrMod (1, "test08"));
        (Ast.AttrExport (3, [("f", 0); ("f", 1); ("f", 2)]));
        (Ast.DeclFun (5, "f", 0,
           [(Ast.ClsFun (5, [], None,
               (Ast.ExprBody
                  [(Ast.ExprLocalCall (6,
                      (Ast.ExprLit
                         (Ast.LitAtom (6, "f"))),
                      [(Ast.ExprLit
                          (Ast.LitInteger (6, 1)))
                        ]
                      ))
                    ])
               ))
             ]
           ));
        (Ast.DeclFun (8, "f", 1,
           [(Ast.ClsFun (8, [(Ast.PatVar (8, "N"))],
               None,
               (Ast.ExprBody
                  [(Ast.ExprRemoteCall (9, 9,
                      (Ast.ExprLit
                         (Ast.LitAtom (9, "test08"))),
                      (Ast.ExprLit
                         (Ast.LitAtom (9, "f"))),
                      [(Ast.ExprVar (9, "N"));
                        (Ast.ExprLit
                           (Ast.LitInteger (9, 2)))
                        ]
                      ))
                    ])
               ))
             ]
           ));
        (Ast.DeclFun (11, "f", 2,
           [(Ast.ClsFun (11,
               [(Ast.PatVar (11, "N"));
                 (Ast.PatVar (11, "M"))],
               None,
               (Ast.ExprBody
                  [(Ast.ExprBinOp (11, "+",
                      (Ast.ExprVar (11, "N")),
                      (Ast.ExprVar (11, "M"))))
                    ])
               ))
             ]
           ));
        Ast.FormEof])
   |> Result.return

let rec suite =
  "parse_abs_form_in_beam_suite" >:::
    [
      "template_01" >:: assert_equals_abs_form_f template_01 "test01.beam";
      "template_02" >:: assert_equals_abs_form_f template_02 "test02.beam";
      "template_03" >:: assert_equals_abs_form_f template_03 "test03.beam";
      "template_04" >:: assert_equals_abs_form_f template_04 "test04.beam";
      "template_05" >:: assert_equals_abs_form_f template_05 "test05.beam";
      "template_06" >:: assert_equals_abs_form_f template_06 "test06.beam";
      "template_07" >:: assert_equals_abs_form_f template_07 "test07.beam";
      "template_08" >:: assert_equals_abs_form_f template_08 "test08.beam";
    ]

let () =
  run_test_tt_main suite
