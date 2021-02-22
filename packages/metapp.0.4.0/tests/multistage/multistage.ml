[%%metadef
  [%%metadef
     let world () =
       Metapp_preutils.Exp.of_string "world"]

  let hello () =
    Metapp_preutils.Exp.of_string (
      Printf.sprintf "Hello, %s!" [%meta world ()])]

let () =
  Test_framework.assert_eq String.equal Format.pp_print_string
    [%meta hello ()] "Hello, world!"
