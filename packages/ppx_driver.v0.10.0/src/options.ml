let perform_checks = true
(* The checks on extensions are only to get better error messages
   since the compiler will choke on unknown extensions. We disable
   them externally to make it easier to use non ppx_driver based
   rewriters with ppx_driver *)
let perform_checks_on_extensions = false
let diff_command = None
