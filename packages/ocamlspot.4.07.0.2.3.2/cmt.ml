(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2014 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

open Utils

open Cmt_format

let source_path file = 
  Option.map file.cmt_sourcefile ~f:(fun f -> file.cmt_builddir ^/ f)

(* xxx.{ml,cmo,cmx,spot} => xxx.cmt
   xxx.{mli,cmi,spit}    => xxx.cmti *)
let of_path path =
  let loc = Pathmap.build_loc path in
  Format.eprintf "Cmt.of_path: Pathmap build: %s => %s@." path loc;
  let cands = match Filename.split_extension path with
    | _, (".cmi" | ".cmti") -> [ loc ^ ".cmti" ]
    | _, (".cmo" | ".cmx" | ".cmt") -> [ loc ^ ".cmt" ]
    | _base, ".mli" -> [ loc ^ ".cmti" ]
    | _base, _ -> [ loc ^ ".cmt" ]
  in
  let rec find = function
    | [] -> assert false
    | [p] -> p
    | p::ps -> 
        Format.eprintf "trying %s@." p;
        if Sys.file_exists p then  p
        else find ps
  in
  let s = find cands in
  Format.eprintf "=> %s@." s;
  s


(* CR jfuruse: this is a dirty workaround. It should be nice if we could know cmt is created by opt or byte *)          
let is_opt cmt = 
  (* We cannot guess this simply by the compiler name "ocamlc" or "ocamlopt", 
     since someone can create a modified compiler like gcaml *)
  List.exists (fun x -> match Filename.split_extension x with 
    | (_, ".cmx") -> true 
    | _ -> false) (Array.to_list cmt.cmt_args)

let reset_env_cache () = Envaux.reset_cache ()

let recover_env env = 
  Envaux.reset_cache (); (* reset required for machines with small memory... *)
  Envaux.env_from_summary (Env.summary env) Subst.identity
