module Compiler_modules = struct
  module Toploop = Toploop
  module Topdirs = Topdirs
end

open Compiler_modules

let unsafe_string () = !Clflags.unsafe_string

module type S = sig
  val dir_directory : string -> unit
  val dir_install_printer : Format.formatter -> Longident.t -> unit
end

let init ~native:_ (module M : S) =
  Topfind.log := ignore;
  Topfind.don't_load_deeply ["toplevel_expect_test"];
  Topfind.add_predicates ["byte"; "toploop"; "ppx_driver"; "mt"; "mt_posix"];
  (* Add findlib path so Topfind is available and it won't be initialized twice if the
     user does [#use "topfind"]. *)
  Topdirs.dir_directory (Findlib.package_directory "findlib")
;;
