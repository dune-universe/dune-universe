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
open Common

(* Different implementation of realize. *)
let realize_data =
  [
    ("Stub.Native", GettextStub.Native.realize);
    ("Stub.Preload", GettextStub.Preload.realize);
  ]

(************************************************)
(* Set bad locale and spot error when setlocale *)
(* returns NULL                                 *)
(************************************************)

let bad_setlocale =
  "Call setlocale with bad locale"
  >::: [
         ( "setlocale with bad locale" >:: fun _ ->
           ignore (GettextStubCompat.setlocale GettextStubCompat.LC_ALL "xx")
         );
       ]

let () =
  run_test_tt_main
    ("test-stub" >::: [
        bad_setlocale;
        implementation_test realize_data;
      ])
