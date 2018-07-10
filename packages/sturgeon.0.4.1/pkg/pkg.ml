#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "sturgeon" @@ fun c ->
  Ok [ Pkg.mllib "src/sturgeon.mllib"
     ; Pkg.mllib "src/sturgeon_recipes_command.mllib"
     ; Pkg.mllib "src/sturgeon_recipes_server.mllib"
     ; Pkg.lib "sturgeon.top"
     ; Pkg.bin ~dst:"sturgeon-connector" "src/sturgeon_connector"
     ; Pkg.share_root ~dst:"emacs/site-lisp/sturgeon.el" "emacs/sturgeon.el"
     ]
