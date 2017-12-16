#!/usr/bin/env ocaml
#use "topfind"
#require "topkg-jbuilder"

let _ =
Topkg_jbuilder.describe ~name:"mirage-net-xen" ()

