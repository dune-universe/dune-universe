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

open Benchmark
open Common
open GettextTypes

type benchs = { verbose : bool; search_path : string list; time : int }

(* Different implementation of realize. *)
let realize_data =
  [
    ("Camomile.Map", GettextCamomile.Map.realize);
    ("Camomile.Hashtbl", GettextCamomile.Hashtbl.realize);
    ("Camomile.Open", GettextCamomile.Open.realize);
    ("Stub.Native", GettextStub.Native.realize);
    ("Stub.Preload", GettextStub.Preload.realize);
  ]

let parse_arg () =
  let benchs = ref { verbose = false; search_path = []; time = 1 } in
  Arg.parse
    (Arg.align
       [
         ( "--search",
           Arg.String
             (fun dir ->
               benchs :=
                 { !benchs with search_path = dir :: !benchs.search_path }),
           "dir Search the specified directory for MO file." );
         ( "--verbose",
           Arg.Unit (fun () -> benchs := { !benchs with verbose = true }),
           "Processs with a lot of message." );
         ( "--time",
           Arg.Int (fun sec -> benchs := { !benchs with time = sec }),
           Printf.sprintf
             "second Process each test during the specified number of second. \
              Default : %d."
             !benchs.time );
       ])
    (fun _str -> ())
    ( "Benchmark utility for ocaml-gettext v" ^ GettextConfig.version
    ^ " by Sylvain Le Gall\n"
    ^ "Copyright (C) 2004-2008 Sylvain Le Gall <sylvain@le-gall.net>\n"
    ^ "Licensed under LGPL v2.1 with Ocaml exception." );
  !benchs

let print_debug benchs str =
  if benchs.verbose then (
    print_string str;
    print_newline () )
  else ()

let make_buffer lst = (lst, [])

let get_buffer (lst1, lst2) =
  match (lst1, lst2) with
  | hd :: tl, lst2 -> (hd, (tl, hd :: lst2))
  | [], hd :: tl -> (hd, (tl, [ hd ]))
  | [], [] -> failwith "Buffer is empty"

(* Generic function to benchmark gettextCompat function *)
let gettext_bench benchs str_gettext fun_gettext =
  let f ref_translations =
    let (debug_str, t', textdomain, tr), buffer =
      get_buffer !ref_translations
    in
    print_debug benchs
      (Printf.sprintf "Translation of %S from %s" (string_of_translation tr)
         debug_str);
    ignore (fun_gettext t' textdomain tr);
    ref_translations := buffer
  in
  let parameters_lst = List.map parameters_of_filename mo_files_data in
  let create_translation (str_realize, realize) =
    let rec create_one_translation accu lst =
      match lst with
      | parameters :: tl ->
          let t = t_of_parameters parameters in
          let t' = realize t in
          let new_accu =
            List.fold_left
              (fun lst tr ->
                ( str_realize ^ " with textdomain " ^ parameters.textdomain,
                  t',
                  parameters.textdomain,
                  tr )
                :: lst)
              accu parameters.translations
          in
          create_one_translation new_accu tl
      | [] -> make_buffer accu
    in
    ref (create_one_translation [] parameters_lst)
  in
  let bench_lst =
    List.map
      (fun (str_realize, realize) ->
        (str_realize, f, create_translation (str_realize, realize)))
      realize_data
  in
  print_debug benchs ("Benchmarking " ^ str_gettext ^ ":");
  (str_gettext ^ " benchmark", throughputN benchs.time bench_lst)

(*******************************)
(* Performance of check_format *)
(*******************************)

let format_bench benchs =
  let f ref_buffer =
    let elem, buffer = get_buffer !ref_buffer in
    let translation =
      print_debug benchs ("Checking format of : " ^ string_of_translation elem);
      GettextFormat.check_format Ignore elem
    in
    print_debug benchs
      ("Result of the check : " ^ string_of_translation translation);
    ref_buffer := buffer
  in
  print_debug benchs "Benchmarking format :";
  ( "Format benchmark",
    throughputN benchs.time
      [
        ("Singular", f, ref (make_buffer format_translation_singular_data));
        ("Plural", f, ref (make_buffer format_translation_plural_data));
        ("All", f, ref (make_buffer format_translation_all_data));
      ] )

(***************************)
(* Performance of realize  *)
(***************************)

let realize_bench benchs =
  let f (realize, parameters_lst) =
    let f_one parameters =
      let t =
        print_debug benchs ("Creating t for " ^ parameters.fl_mo);
        t_of_parameters parameters
      in
      print_debug benchs ("Realizing t for " ^ parameters.fl_mo);
      ignore (realize t)
    in
    List.iter f_one parameters_lst
  in
  let parameters_lst = List.map parameters_of_filename mo_files_data in
  let bench_lst =
    List.map
      (fun (str_implementation, realize) ->
        (str_implementation, f, (realize, parameters_lst)))
      realize_data
  in
  print_debug benchs "Benchmarking realize:";
  ("Realize benchmark", throughputN benchs.time bench_lst)

(**********************)
(* Performance of s_  *)
(**********************)

let s_bench benchs =
  let fun_gettext t' textdomain translation =
    match translation with
    | Singular (str, _) -> ignore (GettextCompat.dgettext t' textdomain str)
    | _ -> ()
  in
  gettext_bench benchs "s_" fun_gettext

(*********************)
(* Performance of f_ *)
(*********************)

let f_bench benchs =
  let fun_gettext t' textdomain translation =
    match translation with
    | Singular (str, _) ->
        ignore (GettextCompat.fdgettext t' textdomain (Obj.magic str))
    | _ -> ()
  in
  gettext_bench benchs "f_" fun_gettext

(**********************)
(* Performance of sn_ *)
(**********************)

let sn_bench benchs =
  let fun_gettext t' textdomain translation =
    match translation with
    | Plural (str_id, str_plural, _) ->
        ignore (GettextCompat.dngettext t' textdomain str_id str_plural 0);
        ignore (GettextCompat.dngettext t' textdomain str_id str_plural 1);
        ignore (GettextCompat.dngettext t' textdomain str_id str_plural 2);
        ignore (GettextCompat.dngettext t' textdomain str_id str_plural 3)
    | _ -> ()
  in
  gettext_bench benchs "sn_" fun_gettext

(**********************)
(* Performance of fn_ *)
(**********************)

let fn_bench benchs =
  let fun_gettext t' textdomain translation =
    match translation with
    | Plural (str_id, str_plural, _) ->
        ignore
          (GettextCompat.fdngettext t' textdomain (Obj.magic str_id)
             (Obj.magic str_plural) 0);
        ignore
          (GettextCompat.fdngettext t' textdomain (Obj.magic str_id)
             (Obj.magic str_plural) 1);
        ignore
          (GettextCompat.fdngettext t' textdomain (Obj.magic str_id)
             (Obj.magic str_plural) 2);
        ignore
          (GettextCompat.fdngettext t' textdomain (Obj.magic str_id)
             (Obj.magic str_plural) 3)
    | _ -> ()
  in
  gettext_bench benchs "fn_" fun_gettext

(**************************)
(* Main benchmark routine *)
(**************************)

;;
let benchs = parse_arg () in
let all_bench =
  [ format_bench; realize_bench; s_bench; f_bench; sn_bench; fn_bench ]
in
print_env "benchmarks";

(* Running *)
let all_results = List.map (fun x -> x benchs) all_bench in
List.iter
  (fun (str, results) ->
    print_newline ();
    print_newline ();
    print_endline str;
    tabulate results)
  all_results
