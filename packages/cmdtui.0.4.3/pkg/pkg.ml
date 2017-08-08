#!/usr/bin/env ocaml
#use "topfind"
#require "topkg-jbuilder"

let () =
  let uri =
    match Topkg.OS.File.read "_build/release.uri" with
    | Ok uri -> Some uri
    | Error _ -> None in
  let distrib = Topkg.Pkg.distrib ?uri () in
  Topkg_jbuilder.describe ~distrib ()
