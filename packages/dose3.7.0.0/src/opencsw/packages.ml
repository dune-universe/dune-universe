(**************************************************************************)
(*  This file is part of a library developed with the support of the      *)
(*  Mancoosi Project. http://www.mancoosi.org                             *)
(*                                                                        *)
(*  Main author(s):  Pietro Abate                                         *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.  A special linking    *)
(*  exception to the GNU Lesser General Public License applies to this    *)
(*  library, see the COPYING file for more information.                   *)
(**************************************************************************)

module Pcre = Re_pcre
open ExtLib
open Dose_common
open Dose_extra

include Util.Logging (struct
  let label = "dose_opencsw.packages"
end)

type name = string

type version = string

type vpkg = string

type package =
  { name : name;
    version : version;
    depends : vpkg list;
    conflicts : vpkg list;
    extras : (string * string) list
  }

let default_package =
  { name = ""; version = ""; depends = []; conflicts = []; extras = [] }

module Set = Set.Make (struct
  type t = package

  let compare p1 p2 = compare (p1.name, p1.version) (p2.name, p2.version)
end)

let input_raw_priv parse_packages files =
  let timer = Util.Timer.create "dose_opencsw.packages" in
  Util.Timer.start timer ;
  if List.length files > 1 then info "Merging input lists" ;
  let s =
    List.fold_left
      (fun acc f ->
        info "Parsing %s..." f ;
        let l = parse_packages (fun x -> x) f in
        List.fold_left (fun s x -> Set.add x s) acc l)
      Set.empty
      files
  in
  info "total packages %n" (Set.cardinal s) ;
  Util.Timer.stop timer (Set.elements s)

exception Eof

let parse_paragraph pkg ch =
  let line =
    try IO.read_line ch with
    | IO.No_more_input -> raise Eof
    | End_of_file -> assert false
  in
  if ExtString.String.strip line = "" then None
  else if ExtString.String.starts_with line "#" then None
  else if
    (* XXX a very crude way of skipping the signature of the file *)
    (* here we assume a specific structure *)
    ExtString.String.starts_with line "-----BEGIN PGP SIGNED MESSAGE-----"
  then None
  else if ExtString.String.starts_with line "Hash: SHA1" then None
  else if ExtString.String.starts_with line "-----BEGIN PGP SIGNATURE-----" then
    raise Eof
  else
    let split s = ExtString.String.nsplit s "|" in
    let catcherr a i =
      try match split a.(i) with ["none"] -> [] | l -> l
      with Invalid_argument _ -> []
    in
    let a = Array.of_list (ExtString.String.nsplit line " ") in
    (*
    debug line;
    Array.iteri (fun i s -> debug "%d : %s" i s) a ;
    *)
    Some
      { pkg with
        name = a.(2);
        version = List.hd (Pcre.split ~rex:(Pcre.regexp ",") a.(1));
        depends = catcherr a 6;
        conflicts = catcherr a 8
      }

let rec parse_packages_rec acc ch =
  try
    match parse_paragraph default_package ch with
    | None -> parse_packages_rec acc ch
    | Some par -> parse_packages_rec (par :: acc) ch
  with Eof -> acc

let parse_packages_in _f ch = parse_packages_rec [] ch

let parse_packages _f filename =
  try
    let ch = Input.open_file filename in
    let l = parse_packages_rec [] ch in
    Input.close_ch ch ;
    l
  with Input.File_empty -> []

let input_raw files = input_raw_priv parse_packages files
