module C = Configurator.V1

let has_lame_h_code =
  {|
#include <lame/lame.h>

int main()
{
  lame_init();
  return 0;
}
|}

let link_flags = ["-lmp3lame"; "-lm"]

let () =
  C.main ~name:"ocaml-lame" (fun c ->
      assert (C.c_test ~link_flags c has_lame_h_code);
      C.Flags.write_sexp "c_flags.sexp" [];
      C.Flags.write_sexp "c_library_flags.sexp" link_flags)
