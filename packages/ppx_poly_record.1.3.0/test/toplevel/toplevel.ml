#!/usr/bin/env ocaml
#use "topfind"
#require "ppx_poly_record"
let f = [%poly_record fun r -> r.x]
