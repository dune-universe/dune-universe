(**************************************************************************)
(*  ocaml-gettext: a library to translate messages                        *)
(*                                                                        *)
(*  Copyright (C) 2003-2008 Sylvain Le Gall <sylvain@le-gall.net>         *)
(*                                                                        *)
(*  This library is free software; you can redistribute it and/or         *)
(*  modify it under the terms of the GNU Lesser General Public            *)
(*  License as published by the Free Software Foundation; either          *)
(*  version 2.1 of the License, or (at your option) any later version;    *)
(*  with the OCaml static compilation exception.                          *)
(*                                                                        *)
(*  This library is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Lesser General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Lesser General Public      *)
(*  License along with this library; if not, write to the Free Software   *)
(*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *)
(*  USA                                                                   *)
(**************************************************************************)

open OUnit2
open FileUtil
open FilePath
open GettextTypes
open GettextCategory
open Common

let load_mo_file ctxt f_test_mo fl_mo =
  let mo = open_in_bin fl_mo in
  let mo_header = GettextMo.input_mo_header mo in
  let mo_informations =
    GettextMo.input_mo_informations RaiseException mo mo_header
  in
  logf ctxt `Info "%s" (GettextMo.string_of_mo_header mo_header);
  logf ctxt `Info "%s" (GettextMo.string_of_mo_informations mo_informations);
  close_in mo;
  f_test_mo fl_mo

(**********************************)
(* Test of Printf format checking *)
(**********************************)

let format_test =
  let format_test_one (trans_src, trans_dst) =
    let lst_str_equal lst1 lst2 =
      try List.fold_left2 (fun b str1 str2 -> b && str1 = str2) true lst1 lst2
      with Invalid_argument _ -> false
    in
    Printf.sprintf "%s -> %s format checking"
      (string_of_translation trans_src)
      (string_of_translation trans_dst)
    >:: fun _ctxt ->
    let trans_res = GettextFormat.check_format Ignore trans_src in
    match (trans_res, trans_dst) with
    | Singular (str_id1, str1), Singular (str_id2, str2)
      when str_id1 = str_id2 && str1 = str2 ->
        ()
    | Plural (str_id1, str_plural1, lst1), Plural (str_id2, str_plural2, lst2)
      when str_id1 = str_id2 && str_plural1 = str_plural2
           && lst_str_equal lst1 lst2 ->
        ()
    | trans1, trans2 ->
        assert_failure
          ( string_of_translation trans1
          ^ " differs from "
          ^ string_of_translation trans2 )
  in
  "Printf format test"
  >::: List.map format_test_one format_translation_check_data

(**************************)
(* Split plural functions *)
(**************************)

let split_plural_test =
  let split_plural_test_one (str, res_lst) =
    Printf.sprintf "Split plural test %S" str >:: fun _ ->
    let lst = GettextUtils.split_plural str in
    List.iter2
      (fun str1 str2 ->
        if str1 = str2 then ()
        else assert_failure (Printf.sprintf "%S should be %S" str2 str1))
      res_lst lst
  in
  "Split plural test"
  >::: List.map split_plural_test_one
         [ ("%d coffee\000more %d coffee", [ "%d coffee"; "more %d coffee" ]) ]

(*************************)
(* Test of PO processing *)
(*************************)

let po_test =
  let po_test_one f_test_mo fl_po =
    Printf.sprintf "Load and compile %s" fl_po >:: fun ctxt ->
      let tests = make_tests ctxt in
      let fl_mo = concat tests.test_dir (replace_extension fl_po "mo") in
      let fl_po = concat tests.test_dir fl_po in
      let chn = open_in fl_po in
      ignore (GettextPo.input_po chn);
      close_in chn;
      GettextCompile.compile fl_po fl_mo;
      load_mo_file ctxt f_test_mo fl_mo
  in
  "PO processing test"
  >::: List.map (po_test_one ignore)
         [ "test1.po"; "test2.po"; "test3.po"; "test4.po"; "test11.po" ]
       @ [
           po_test_one
             (fun fl_mo ->
               let (), _ =
                 GettextMo.fold_mo RaiseException
                   (fun trslt () ->
                     match trslt with
                     | Singular (id, "")
                     | Plural (id, _, [])
                     | Plural (id, _, [ "" ]) ->
                         assert_failure
                           (Printf.sprintf
                              "%s contains an empty translation for %s" fl_mo
                              id)
                     | _ -> ())
                   () fl_mo
               in
               ())
             "test12.po";
         ]

(*******************************************)
(* Test of OCaml source file PO extraction *)
(*******************************************)

let extract_test =
  let default_options = "" in
  let filename_options = MapString.empty in
  let extract_test_one (fl_ml, contents) =
    fl_ml >::: [
      ("Extracting" >:: fun ctxt ->
          let tests = make_tests ctxt in
          let fl_pot = concat tests.test_dir (replace_extension fl_ml "pot") in
          let fl_ml = concat tests.test_dir fl_ml in
          begin
            try
              let ocaml_xgettext =
                if FilePath.is_relative tests.ocaml_xgettext then
                  FilePath.make_absolute (Sys.getcwd ()) tests.ocaml_xgettext
                else
                  tests.ocaml_xgettext
              in
              (* Extract data from files *)
              GettextCompile.extract ocaml_xgettext default_options
                filename_options [ fl_ml ] fl_pot
            with x ->
              assert_failure
                ( fl_ml ^ " doesn't extract correctly: "
                  ^ Gettext.string_of_exception x ) 
          end;

          (* Load POT file *)
          let po =
            let chn = open_in fl_pot in
            let res = GettextPo.input_po chn in
            close_in chn;
            res
          in
          (* Check result *)
          List.iter
            (fun str ->
               if MapString.mem str po.no_domain then ()
               else
                 assert_failure
                   (Printf.sprintf "Cannot find %S in %s" str fl_pot))
            contents );
    ]
in
  "OCaml file extraction test"
  >::: List.map extract_test_one
         [
           ("test4.ml", []);
           ( "escape-char.ml",
             [
               "hello world!\n";
               "goodbye world!\n";
               "goodby world 2!";
               "and then\tbye-bye";
             ] );
         ]

(********************************)
(* Test of MO file installation *)
(********************************)

let install_test =
  let install_test_one (language, category, textdomain, fl_mo, fl_dsts) =
    fl_mo >:: fun ctxt ->
      let tests = make_tests ctxt in
      let fl_mo = concat tests.test_dir fl_mo in
      let fl_dst = make_filename (tests.install_dir :: fl_dsts) in
      GettextCompile.install true tests.install_dir language category textdomain
        fl_mo;
      assert_bool
        (Printf.sprintf "%s is not installed at %s" fl_mo fl_dst)
        (test Exists fl_dst)
  in
  let install_fail_test_one (fl_mo, exc) =
    let error = Printexc.to_string exc in
    Printf.sprintf "%s (%s)" fl_mo error >:: fun ctxt ->
      let tests = make_tests ctxt in
      let fl_mo = concat tests.test_dir fl_mo in
      assert_raises
        ~msg:
          (Printf.sprintf "Installation of %s should have failed with error %s"
             fl_mo error) exc (fun () ->
              GettextCompile.install true tests.install_dir "fr" LC_MESSAGES
                "gettext-fail" fl_mo)
  in
  let install_warning_test_one
      (language, category, textdomain, fl_mo, exp_err, fl_dsts) =
    Printf.sprintf "%s warning" fl_mo >:: fun ctxt ->
      let tests = make_tests ctxt in
      let out = Buffer.create 13 in
      let capture_out strm = Stream.iter (Buffer.add_char out) strm in
      let fl_mo = concat tests.test_dir fl_mo in
      let fl_dst = make_filename (tests.install_dir :: fl_dsts) in
        assert_command
          ~ctxt
          ~use_stderr:true
          ~foutput:capture_out
          tests.ocaml_gettext
          [
            "--action"; "install";
            "--install-language"; language;
            "--install-category"; category;
            "--install-textdomain"; textdomain;
            "--install-destdir"; tests.install_dir;
            fl_mo;
          ];
      assert_equal
        ~msg:("error output")
        ~printer:(Printf.sprintf "%S") exp_err (Buffer.contents out);
      assert_bool
        (Printf.sprintf "File %s doesn't exist" fl_dst)
        (test Exists fl_dst)
  in
  "MO file installation test"
  >::: List.map install_test_one
         [
           ( "fr",
             LC_MESSAGES,
             "gettext-test1",
             "test1.mo",
             [ "fr"; "LC_MESSAGES"; "gettext-test1.mo" ] );
           ( "fr_FR",
             LC_MESSAGES,
             "gettext-test2",
             "test2.mo",
             [ "fr_FR"; "LC_MESSAGES"; "gettext-test2.mo" ]
           );
           ( "fr",
             LC_TIME,
             "gettext-test3",
             "test3.mo",
             [ "fr"; "LC_TIME"; "gettext-test3.mo" ] );
           ( "fr_FR@euro",
             LC_MESSAGES,
             "gettext-test4",
             "test4.mo",
             [ "fr_FR@euro"; "LC_MESSAGES"; "gettext-test4.mo" ] );
         ]
       @ List.map install_fail_test_one
           [
             ("test5.mo", MoInvalidFile);
             ( "test6.mo",
               MoInvalidHeaderTableStringOutOfBound ((28l, 2626l), (-1l, 159l))
             );
             ( "test7.mo",
               MoInvalidHeaderTableTranslationOutOfBound
                 ((28l, 2626l), (-49l, 111l)) );
             ("test8.mo", MoInvalidStringOutOfBound (2626, 36));
             ("test9.mo", MoInvalidTranslationOutOfBound (2626, 196));
           ]
       @ List.map install_warning_test_one
           [
             ( "fr",
               "LC_MESSAGES",
               "test10",
               "test10.mo",
               "Error while processing parsing of plural at line 1 character \
                10: \" nplurals=INTEGER; plural=EXPRESSION;\".\n",
               [ "fr"; "LC_MESSAGES"; "test10.mo" ] );
           ]

(************************)
(* Test of POT/PO merge *)
(************************)

let merge_test =
  let merge_one (fl_pot, fl_po, backup_ext) =
    fl_pot ^ "+" ^ fl_po
    >::: [
      ("Merging" >:: fun ctxt ->
          let tests = make_tests ctxt in
          let fl_pot = concat tests.test_dir fl_pot in
          let fl_po = concat tests.test_dir fl_po in

          try
            (* Copying the file to the good place *)
            let fl_backup = add_extension fl_po backup_ext in
            cp [ fl_po ] fl_pot;
            GettextCompile.merge fl_pot [ fl_po ] backup_ext;
            ( match cmp fl_po fl_po with
              | Some -1 ->
                assert_failure (fl_po ^ " or " ^ fl_po ^ " doesn't exist")
              | Some _ -> assert_failure (fl_po ^ " differs from " ^ fl_po)
              | None -> () );
            match cmp fl_po fl_backup with
            | Some -1 ->
              assert_failure
                (fl_po ^ " or " ^ fl_backup ^ " doesn't exist")
            | Some _ -> assert_failure (fl_po ^ " differs from " ^ fl_backup)
            | None -> ()
          with x ->
            assert_failure
              ( "Unexpected error while processing " ^ fl_po ^ " ( "
                ^ Printexc.to_string x ^ " )" ) );
    ]
  in
  "POT/PO file merge test"
  >::: List.map merge_one
         [ ( "test4.pot", "test4.po", "bak" ); ]

(**********************************)
(* Test for PO processing comment *)
(**********************************)

let po_process_test =
  let copy_merge_compare fn_po =
    let src_po = make_filename [ current_dir; fn_po ] in
    let tgt_po = make_filename [ current_dir; fn_po ] in
    GettextCompile.merge tgt_po [ tgt_po ] "bak";
    match cmp tgt_po src_po with
    | Some -1 -> assert_failure (tgt_po ^ " or " ^ src_po ^ " doesn't exist")
    | Some _ -> assert_failure (tgt_po ^ " differs from " ^ src_po)
    | None -> ()
  in
  "Gettext po process test"
  >::: [
    ("multiline-comment.po" >:: fun ctxt ->
        let tests = make_tests ctxt in
        copy_merge_compare (concat tests.test_dir "multiline-comment.po") );
    ("utf8-fr.po" >:: fun ctxt ->
        let tests = make_tests ctxt in
        copy_merge_compare (concat tests.test_dir "utf8-fr.po") );
    ("utf8-ja.po" >:: fun ctxt ->
        let tests = make_tests ctxt in
        copy_merge_compare (concat tests.test_dir "utf8-ja.po") );
  ]

(********************************************)
(* Test for running ocaml-gettext program   *)
(* (spot also problem with runtime behavior *)
(* of ocaml-gettext library)                *)
(********************************************)

let run_ocaml_gettext =
  "Running ocaml-gettext"
  >::: [
    ( "ocaml-gettext with LC_ALL and LANG unset" >:: fun ctxt ->
          let tests = make_tests ctxt in
          assert_command ~ctxt tests.ocaml_gettext [ "--help" ])
  ]

(******************)
(* Try to compile *)
(******************)

let compile_ocaml =
  "Compile OCaml code"
  >::: List.map
         (fun (bn, exp_return_code, exp_err) ->
           bn >:: fun ctxt ->
            let tests = make_tests ctxt in
            let opt_options =
              let major, minor =
                Scanf.sscanf Sys.ocaml_version "%d.%d.%d" (fun x y _ -> (x, y))
              in
              if (major, minor) >= (4, 3) then
                ["-color"; "never"]
              else
                []
            in
            let out = Buffer.create 13 in
            let capture_out strm = Stream.iter (Buffer.add_char out) strm in
            let match_exp_err = Str.regexp (".*"^(Str.quote exp_err)^".*") in
            assert_command
              ~exit_code:(Unix.WEXITED exp_return_code)
              ~use_stderr:true
              ~foutput:capture_out
              ~ctxt
              "ocamlc"
              (opt_options @ [
                  "-c";
                  "-I";
                  make_filename
                    [Filename.parent_dir_name;
                     "src"; "lib"; "gettext"; "base"; ".gettextBase.objs"; "byte"];
                  "-I"; tests.test_dir;
                  Filename.concat tests.test_dir "TestGettext.ml";
                  Filename.concat tests.test_dir bn;
                ]);
            FileUtil.rm
              (List.map (FilePath.replace_extension bn) [ "cmo"; "cmi" ]);
            FileUtil.rm
              (List.map (FilePath.add_extension "TestGettext") [ "cmo"; "cmi" ]);
            assert_bool
              (Printf.sprintf
                 "error output:\nwant to contain: %S\ngot:\n%s"
                 exp_err (Buffer.contents out))
              (Str.string_match match_exp_err (Buffer.contents out) 0))
         [
           ("unsound_warning.ml", 0, "");
           ("valid_format.ml", 0, "");
           ("invalid_format1.ml", 2, "line 4, characters 28-29:");
           ("invalid_format2.ml", 2, "line 4, characters 28-29:");
           ("invalid_format3.ml", 2, "line 4, characters 27-28:");
           ("invalid_format4.ml", 2, "line 4, characters 27-28:");
           ("invalid_format5.ml", 2, "line 4, characters 36-52:");
         ]

(*********************)
(* Main test routine *)
(*********************)

let () =
  run_test_tt_main
    ("Test ocaml-gettext"
     >::: [
       format_test;
       split_plural_test;
       po_test;
       extract_test;
       install_test;
       po_process_test;
       merge_test;
       run_ocaml_gettext;
       compile_ocaml;
     ])
