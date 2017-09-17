(* Copyright 2016-2017 Vincent Jacques <vincent@vincent-jacques.net> *)

open General.Abbr
open Tst

let () =
  let argv = Li.of_array OCamlStandard.Sys.argv in
  Exit.exit (command_line_main ~argv Hashids.Tests.test)
