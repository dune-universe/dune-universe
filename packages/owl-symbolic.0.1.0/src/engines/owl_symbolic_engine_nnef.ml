(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

type t = string

let of_symbolic x = Obj.magic x

let to_symbolic x = Obj.magic x

let save x filename = Obj.magic (x, filename)

let load filename = Obj.magic filename
