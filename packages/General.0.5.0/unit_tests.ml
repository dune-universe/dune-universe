open General.Abbr

let () =
  let argv = Li.of_array OCamlStandard.Sys.argv in
  Exit.exit (Tst.command_line_main ~argv General.Tests.test)
