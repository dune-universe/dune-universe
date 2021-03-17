module C = Configurator.V1

let has_lo_h_code =
  {|
#include <stdio.h>
#include <lo/lo.h>

int main()
{
  lo_address_new_with_proto(0, NULL, NULL);
  return 0;
}
|}

let link_flags = ["-llo"]

let () =
  C.main ~name:"lo-config" (fun c ->
      assert (C.c_test ~link_flags c has_lo_h_code);
      C.Flags.write_sexp "c_flags.sexp" [];
      C.Flags.write_sexp "c_library_flags.sexp" link_flags)
