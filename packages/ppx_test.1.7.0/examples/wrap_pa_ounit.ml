module Ptest = struct
  open Pa_ounit.Runtime (* need pa_ounit *)
  open Ppx_test.Location (* need ppx_test *)
  open Lexing

  let fix_name = function
    | None -> "<noname>"
    | Some n -> n

  let test loc name f = 
    Pa_ounit.Runtime.test (fix_name) name loc.loc_start.pos_fname
      loc.loc_start.pos_lnum
      (loc.loc_start.pos_cnum - loc.loc_start.pos_bol)
      (loc.loc_end.pos_cnum - loc.loc_start.pos_bol)
      f

  let test_unit loc name f = 
    Pa_ounit.Runtime.test_unit (fix_name) name loc.loc_start.pos_fname
      loc.loc_start.pos_lnum
      (loc.loc_start.pos_cnum - loc.loc_start.pos_bol)
      (loc.loc_end.pos_cnum - loc.loc_start.pos_bol)
      f
end
