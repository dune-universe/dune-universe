(* This file is part of ocaml-crontab.
 *
 * Copyright (C) 2019 Yann Régis-Gianas
 *
 * ocaml-crontab is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

let rec read_all b cin =
  try Buffer.add_channel b cin 1; read_all b cin
  with End_of_file -> Buffer.contents b
