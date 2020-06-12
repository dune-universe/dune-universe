module C = Configurator.V1

let gettext_code = {|
#include <libintl.h>

int main() {
  gettext("abcd");
  return 0;
}
|}

let () =
  C.main ~name:"gettext" (fun c ->
      let is_working (c_flags, link_flags) =
        C.c_test c gettext_code ~link_flags ~c_flags
      in
      let c_flags, link_flags =
        try
          List.find
            is_working
            [
              (* Default that should work on standard Linux distributions. *)
              ([], []);

              (* MacOS with Homebrew.
               * The library is "keg-only" to prevent conflict with system
               * installed BSD gettext library, so we have to pull it from
               * /usr/local.
               * https://formulae.brew.sh/formula/gettext
               *)
              (["-I/usr/local/opt/gettext/include"],
               ["-L/usr/local/opt/gettext/lib"; "-lintl"]);

              (* MacOS with MacPorts.
               * -- This is untested, a patch is welcome if you use MacPorts --
               * https://ports.macports.org/port/gettext/summary
               *)
              (["-I/usr/local/include"],
               ["-L/usr/local/lib"; "-lintl"]);
            ]
        with Not_found ->
          C.die "no ways to compile with gettext library"
      in
      C.Flags.write_sexp "c_flags.sexp" c_flags;
      C.Flags.write_sexp "c_library_flags.sexp" link_flags)
