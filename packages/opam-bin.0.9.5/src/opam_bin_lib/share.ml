(**************************************************************************)
(*                                                                        *)
(*    Copyright 2020 OCamlPro & Origin Labs                               *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open EzCompat

(*
open Ezcmd.TYPES
open EzConfig.OP
open OpamParserTypes

*)

open EzFile.OP

let exit = Exit

let check_sharable file =
  let basename = Filename.basename file in
  match EzFile.last_extension basename with
  | None -> begin
      match String.lowercase basename with
      | "license"
      | "changes"
      | "meta"
      | "opam"
      | "dune-package" ->
        true
      | _ ->
        match
          let ic = open_in file in
          let bytes = Bytes.create 256 in
          let len = input ic bytes 0 256 in
          for i = 0 to len -1 do
            let c = int_of_char ( Bytes.get bytes i ) in
            if c >= 128 then begin
              close_in ic ;
              raise exit
            end
          done;
          close_in ic
        with
        | () ->
          Printf.eprintf "check_sharable: false (no ext, no byte) for %s\n%!" file ;
          false
        | exception Exit -> true
    end
  | Some ext -> match ext with
    | "exe" | "byte" | "opt" | "native"
    | "a" | "so" | "o"
    | "cmi" | "cma" | "cmo" | "cmx" | "cmxs" | "cmxa"
    | "cmt" | "cmti"
    | "ml" | "mli"
    | "html" | "md" | "mld"
    | "png" | "pdf"
    | "h"
    | "js" | "css"
    | "el" | "vim"
    | "1" | "3o" | "5"
    | "cache"
      -> true
    | _ ->
      Printf.eprintf "check_sharable: false (ext %S) for %s\n%!" ext file ;
      false

let share_file ~share_dir file =
  match Digest.file file with
  | exception exn ->
    Misc.global_log "Sharing file %s: exception %s" file
      ( Printexc.to_string exn )
  | md5 ->
    Printf.eprintf "SHARING %s\n%!" file ;
    let hex = Digest.to_hex md5 in
    let dirname =
      share_dir //
      String.make 1 hex.[0] //
      String.make 1 hex.[1] //
      String.make 1 hex.[2]
    in
    let shared_file = dirname // ( hex ^ ".share" ) in
    if Sys.file_exists shared_file then begin
      try
        Sys.remove file ;
        Unix.link shared_file file
      with exn ->
        Printf.kprintf failwith "Sharing file %s: exception %s" file
          ( Printexc.to_string exn )
    end else begin
      EzFile.make_dir ~p:true dirname ;
      Unix.link file shared_file
    end


let files ?( share_dir = Globals.opambin_share_dir ) files =
  EzFile.make_dir ~p:true share_dir ;
  match Unix.stat share_dir with
  | exception exn ->
    Misc.global_log "Warning: sharing disabled, exception %s"
      ( Printexc.to_string exn )
  | { Unix.st_dev = partition_dev ; _ } ->
    List.iter (fun file ->
        match Unix.lstat file with
        | exception exn ->
          Misc.global_log "Sharing file %s: error %s" file
            ( Printexc.to_string exn )
        | st ->
          if st.st_dev <> partition_dev then
            Misc.global_log "Sharing file %s: other partition" file
          else
            match st.st_kind with
            | S_REG ->
              if check_sharable file then
                share_file ~share_dir file
            | _ -> ()
      ) files
