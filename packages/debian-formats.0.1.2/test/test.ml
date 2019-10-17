(******************************************************************************)
(*  ocaml-debian-formats: parse Debian files.                                 *)
(*                                                                            *)
(*  Copyright (C) 2010-2017, Sylvain Le Gall                                  *)
(*                                                                            *)
(*  This library is free software; you can redistribute it and/or modify it   *)
(*  under the terms of the GNU Lesser General Public License as published by  *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at   *)
(*  your option) any later version, with the OCaml static compilation         *)
(*  exception.                                                                *)
(*                                                                            *)
(*  This library is distributed in the hope that it will be useful, but       *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of                *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file         *)
(*  COPYING for more details.                                                 *)
(*                                                                            *)
(*  You should have received a copy of the GNU Lesser General Public License  *)
(*  along with this library; if not, write to the Free Software Foundation,   *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA             *)
(******************************************************************************)

open OUnit2
open DebianFormats

let with_fn fn f test_ctxt =
  let chn =
    bracket
      (fun _ -> open_in (Filename.concat "data" fn))
      (fun chn _ -> close_in chn)
      test_ctxt
  in
  f (IO.input_channel chn)

let tests =
  "DebianFormats"
  >::: [
         "Control"
         >::: List.map
                (fun (fn, f) ->
                  fn >:: with_fn fn (fun ch -> f (Control.parse ch)))
                [
                  ( "control.ocaml-data-notation",
                    fun (src, _) ->
                      assert_equal
                        ~printer:(fun s -> s)
                        "ocaml-data-notation" src.Control.source );
                ];
         "Changelog"
         >::: List.map
                (fun (fn, f) ->
                  fn >:: with_fn fn (fun ch -> f (Changelog.head ch)))
                [
                  ( "changelog.ocaml-data-notation",
                    fun e ->
                      assert_equal ~msg:"source"
                        ~printer:(fun s -> s)
                        "ocaml-data-notation" e.Changelog.source;
                      assert_equal ~msg:"version"
                        ~printer:(fun s -> s)
                        "0.0.3-1" e.Changelog.version );
                ];
         "Watch"
         >::: List.map
                (fun (fn, f) ->
                  fn >:: with_fn fn (fun ch -> f (Watch.parse ch)))
                [ ("watch.oasis", ignore); ("watch.obus", ignore) ];
       ]

let _ = run_test_tt_main tests
