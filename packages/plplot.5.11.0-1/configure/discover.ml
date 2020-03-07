module C = Configurator.V1

let plplot_test = {|
#include <plplot.h>

int main()
{
  printf("PL_X_AXIS = %d\n", PL_X_AXIS);
  return 0;
}
|}

let () =
  C.main ~name:"plplot" (fun c ->
      let conf =
        let default = { C.Pkg_config.cflags = []; libs = ["-lplplot"] } in
        match C.Pkg_config.get c with
        | None -> default
        | Some p -> begin
            match C.Pkg_config.query ~package:"plplot" p with
            | None -> begin
                match C.Pkg_config.query ~package:"plplotd" p with 
                | None -> default 
                | Some conf -> conf
              end
            | Some conf -> conf
          end 
      in
      if not
        @@ C.c_test
          c
          plplot_test
          ~c_flags:conf.cflags
          ~link_flags:conf.libs
      then
        failwith "No valid installation of plplot or plplotd found."
      else
        C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs)