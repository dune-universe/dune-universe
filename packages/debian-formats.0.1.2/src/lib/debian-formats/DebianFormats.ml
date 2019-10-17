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

open DF822
open ExtLib
open DFUtils

type name = string

type version = string

type vpkg = string * (string * string) option

type veqpkg = string * (string * string) option

type architecture = string

type md5sum = Digest.t

type sha1 = string

type sha256 = string

type file_size = int64

type filename = string

(**/**)

let default dflt f1 f2 fld = try f1 f2 fld with Not_found -> dflt

(**/**)

module Version = struct
  let noepoch ver = try snd (String.split ver ":") with Invalid_string -> ver

  let upstream ver =
    try fst (String.split (noepoch ver) "-") with Invalid_string -> ver

  let is_native ver = String.contains ver '-'
end

module Release = struct
  type t = {
    origin : string;
    label : string;
    suite : string;
    version : string;
    codename : string;
    date : string;
    architecture : string;
    component : string;
    description : string;
    md5sums : (md5sum * file_size * string) list;
    sha1 : (sha1 * file_size * string) list;
    sha256 : (sha256 * file_size * string) list;
  }

  let parse ch =
    let parse_release_fields par =
      let parse field =
        try single_line field (List.assoc field par) with Not_found -> ""
      in
      {
        origin = parse "origin";
        label = parse "label";
        suite = parse "suite";
        version = parse "version";
        codename = parse "codename";
        date = parse "date";
        architecture = parse "architecture";
        component = parse "component";
        description = parse "description";
        md5sums = [];
        sha1 = [];
        sha256 = [];
      }
    in
    match parse_paragraph (start_from_channel ch) with
    | None -> raise Not_found
    | Some par -> parse_release_fields par
end

module Source = struct
  type t = {
    name : name;
    version : version;
    binary : vpkg list;
    build_depends : (vpkg * (bool * architecture) list) list list;
    build_depends_indep : (vpkg * (bool * architecture) list) list list;
    build_conflicts : (vpkg * (bool * architecture) list) list;
    build_conflicts_indep : (vpkg * (bool * architecture) list) list;
    architecture : architecture list;
    md5sums : (md5sum * file_size * filename) list;
    sha1 : (sha1 * file_size * filename) list;
    sha256 : (sha256 * file_size * filename) list;
    directory : filename;
    section : string;
  }

  let parse_name = parse_package

  let parse_arch s = List.filter (( <> ) "") (String.nsplit s " ")

  let parse_version s = parse_version s

  let parse_binary s = parse_vpkglist parse_constr s

  let parse_cnf s = parse_vpkgformula parse_builddeps s

  let parse_conj s = parse_vpkglist parse_builddeps s

  let parse_cksum lst =
    List.fold_left
      (fun acc line ->
        match List.filter (( <> ) "") (String.nsplit line " ") with
        | cksum :: sz :: tl ->
            (cksum, Int64.of_string sz, String.concat " " tl) :: acc
        | _ -> acc)
      [] (List.rev lst)

  (* Relationships between source and binary packages
   * http://www.debian.org/doc/debian-policy/ch-relationships.html
   * Build-Depends, Build-Depends-Indep, Build-Conflicts, Build-Conflicts-Indep
   *)
  let parse_sources_fields par =
    let parse_s f field = f (single_line field (List.assoc field par)) in
    let parse_m f field = f (String.concat " " (List.assoc field par)) in
    let parse_l f field = f (List.assoc field par) in
    let exec () =
      {
        name = parse_s parse_name "package";
        version = parse_s parse_version "version";
        architecture = parse_s parse_arch "architecture";
        binary = default [] parse_m parse_binary "binary";
        build_depends = default [] parse_m parse_cnf "build-depends";
        build_depends_indep =
          default [] parse_m parse_cnf "build-depends-indep";
        build_conflicts = default [] parse_m parse_conj "build-conflicts";
        build_conflicts_indep =
          default [] parse_m parse_conj "build-conflicts-indep";
        md5sums = default [] parse_l parse_cksum "files";
        sha1 = default [] parse_l parse_cksum "checksums-sha1";
        sha256 = default [] parse_l parse_cksum "checksums-sha256";
        directory = parse_s (fun s -> String.strip s) "directory";
        section = parse_s (fun s -> String.strip s) "section";
      }
    in
    try Some (exec ()) with Not_found -> None

  (* this package doesn't either have version, arch or name *)

  (** parse a debian Sources file from channel *)
  let parse f ch =
    let parse_packages = parse_822_iter parse_sources_fields in
    parse_packages f (start_from_channel ch)

  let filename t ft =
    let test =
      match ft with
      | `Dsc -> fun s -> String.ends_with s ".dsc"
      | `Tarball ->
          fun s ->
            (not
               ( String.ends_with s ".debian.tar.gz"
               || String.ends_with s ".debian.tar.bz2" ))
            && (String.ends_with s ".tar.gz" || String.ends_with s ".tar.bz2")
      | `Diff ->
          fun s ->
            String.ends_with s ".diff.gz"
            || String.ends_with s ".debian.tar.gz"
            || String.ends_with s ".debian.tar.bz2"
      | `Other fn -> ( = ) fn
    in
    let md5sum, sz, fn = List.find (fun (_, _, fn) -> test fn) t.md5sums in
    let find acc f fld =
      try
        let digest, _, _ = List.find (fun (_, _, fn') -> fn = fn') fld in
        f digest :: acc
      with Not_found -> acc
    in
    let digests = [ `MD5Sum md5sum ] in
    let digests = find digests (fun d -> `Sha1 d) t.sha1 in
    let digests = find digests (fun d -> `Sha256 d) t.sha256 in
    (fn, sz, digests)
end

module Binary = struct
  type t = {
    name : name;
    version : version;
    essential : bool;
    source : name * version option;
    depends : vpkg list list;
    pre_depends : vpkg list list;
    recommends : vpkg list list;
    suggests : vpkg list;
    enhances : vpkg list;
    conflicts : vpkg list;
    breaks : vpkg list;
    replaces : vpkg list;
    provides : veqpkg list;
    extras : (string * string) list;
  }
  (** debian package format *)

  let parse_name = parse_package

  let parse_vpkg = parse_constr

  let parse_veqpkg = parse_constr

  let parse_conj s = parse_vpkglist parse_vpkg s

  let parse_cnf s = parse_vpkgformula parse_vpkg s

  let parse_prov s = parse_veqpkglist parse_veqpkg s

  let parse_essential = function
    | "Yes" | "yes" -> true
    | "No" | "no" -> false (* this one usually is not there *)
    | _ -> assert false

  (* unreachable ?? *)

  let parse_packages_fields extras par =
    let extras = "status" :: extras in
    let parse_s f field = f (single_line field (List.assoc field par)) in
    let parse_m f field = f (String.concat " " (List.assoc field par)) in
    let parse_e extras =
      List.filter_map
        (fun prop ->
          let prop = String.lowercase prop in
          try Some (prop, single_line prop (List.assoc prop par))
          with Not_found -> None)
        extras
    in
    let exec () =
      {
        name = parse_s parse_name "package";
        version = parse_s parse_version "version";
        essential = default false parse_s parse_essential "essential";
        source = default ("", None) parse_s parse_source "source";
        depends = default [] parse_m parse_cnf "depends";
        pre_depends = default [] parse_m parse_cnf "pre-depends";
        recommends = default [] parse_m parse_cnf "recommends";
        suggests = default [] parse_m parse_conj "suggests";
        enhances = default [] parse_m parse_conj "enhances";
        conflicts = default [] parse_m parse_conj "conflicts";
        breaks = default [] parse_m parse_conj "breaks";
        replaces = default [] parse_m parse_conj "replaces";
        provides = default [] parse_m parse_prov "provides";
        extras = parse_e extras;
      }
    in
    (* this package doesn't either have version or name *)
    try Some (exec ()) with Not_found -> None

  (** parse a debian Packages file from the channel [ch] *)
  let parse ?(extras = []) f ch =
    let parse_packages = parse_822_iter (parse_packages_fields extras) in
    parse_packages f (start_from_channel ch)
end

module Control = struct
  type source_section = {
    source : name;
    section : name;
    priority : name;
    maintainer : string;
    uploaders : string list;
    standards_version : version;
    build_depends : (vpkg * (bool * architecture) list) list list;
    build_depends_indep : (vpkg * (bool * architecture) list) list list;
    build_conflicts : (vpkg * (bool * architecture) list) list;
    build_conflicts_indep : (vpkg * (bool * architecture) list) list;
  }
  (** debian source section format *)

  type binary_section = {
    package : name;
    essential : bool;
    depends : vpkg list list;
    pre_depends : vpkg list list;
    recommends : vpkg list list;
    suggests : vpkg list;
    enhances : vpkg list;
    conflicts : vpkg list;
    breaks : vpkg list;
    replaces : vpkg list;
    provides : veqpkg list;
    extras : (string * string) list;
  }
  (** debian binary sections format *)

  type t = source_section * binary_section list

  let parse_name = parse_package

  let parse_vpkg = parse_virtual_constr

  let parse_veqpkg = parse_virtual_constr

  let parse_conj s = parse_vpkglist parse_vpkg s

  let parse_essential = function
    | "Yes" | "yes" -> true
    | "No" | "no" -> false (* this one usually is not there *)
    | _ -> assert false

  (* unreachable ?? *)

  let parse_source_fields par =
    let parse_cnf s = parse_vpkgformula parse_builddeps s in
    let parse_conj s = parse_vpkglist parse_builddeps s in
    let parse_s f field =
      try f (single_line field (List.assoc field par))
      with Not_found ->
        failwith (Printf.sprintf "cannot find single line field %S" field)
    in
    let parse_m f field = f (String.concat " " (List.assoc field par)) in
    let exec () =
      {
        source = parse_s parse_name "source";
        section = parse_s parse_name "section";
        (* TODO: be more precise *)
        priority = parse_s parse_name "priority";
        (* TODO: more precise *)
        maintainer = "";
        (* TODO *)
        uploaders = [];
        (* TODO *)
        standards_version = parse_s parse_version "standards-version";
        build_depends = default [] parse_m parse_cnf "build-depends";
        build_depends_indep =
          default [] parse_m parse_cnf "build-depends-indep";
        build_conflicts = default [] parse_m parse_conj "build-conflicts";
        build_conflicts_indep =
          default [] parse_m parse_conj "build-conflicts-indep";
      }
    in
    Some (exec ())

  let parse_binary_fields extras par =
    let extras = "status" :: extras in
    let parse_s f field = f (single_line field (List.assoc field par)) in
    let parse_m f field = f (String.concat " " (List.assoc field par)) in
    let parse_cnf s = parse_vpkgformula parse_vpkg s in
    let parse_prov s = parse_veqpkglist parse_veqpkg s in
    let parse_e extras =
      List.filter_map
        (fun prop ->
          let prop = String.lowercase prop in
          try Some (prop, single_line prop (List.assoc prop par))
          with Not_found -> None)
        extras
    in
    let exec () =
      {
        package = parse_s parse_name "package";
        essential = default false parse_s parse_essential "essential";
        (* TODO: handle ${ ... } *)
        depends = (try default [] parse_m parse_cnf "depends" with _ -> []);
        pre_depends = default [] parse_m parse_cnf "pre-depends";
        recommends = default [] parse_m parse_cnf "recommends";
        suggests = default [] parse_m parse_conj "suggests";
        enhances = default [] parse_m parse_conj "enhances";
        conflicts = default [] parse_m parse_conj "conflicts";
        breaks = default [] parse_m parse_conj "breaks";
        replaces = default [] parse_m parse_conj "replaces";
        (* TODO: handle ${ ... } *)
        provides = (try default [] parse_m parse_prov "provides" with _ -> []);
        extras = parse_e extras;
      }
    in
    (* this package doesn't either have version or name *)
    try Some (exec ()) with Not_found -> None

  let parse chn =
    let ch = start_from_channel chn in
    let src =
      match parse_paragraph ch with
      | Some par -> (
          match parse_source_fields par with
          | Some src -> src
          | None -> failwith "Malformed source package" )
      | None -> failwith "No source package"
    in
    let binaries = parse_822_iter (parse_binary_fields []) (fun s -> s) ch in
    (src, binaries)

  let filename = Filename.concat "debian" "control"

  let default () = with_fn filename parse
end

module Changelog = DFChangelog
module Watch = DFWatch

module URI = struct
  type uri = string

  type mirror = uri

  type dist = string

  type section = [ `Main | `Contrib | `NonFree ]

  (**/**)

  let concat uri1 uri2 =
    match (String.ends_with uri1 "/", String.starts_with uri2 "/") with
    | true, true -> uri1 ^ String.lchop uri2
    | false, true | true, false -> uri1 ^ uri2
    | false, false -> uri1 ^ "/" ^ uri2

  let rec concat_lst = function
    | uri1 :: uri2 :: tl -> concat_lst (concat uri1 uri2 :: tl)
    | [ uri ] -> uri
    | [] -> ""

  let string_of_section = function
    | `Main -> "main"
    | `Contrib -> "contrib"
    | `NonFree -> "non-free"

  (**/**)

  let sources mirror dist section =
    concat_lst
      [
        mirror;
        "dists";
        dist;
        string_of_section section;
        "source/Sources.bz2";
      ]

  let pool mirror src fn = concat_lst [ mirror; src.Source.directory; fn ]
end
