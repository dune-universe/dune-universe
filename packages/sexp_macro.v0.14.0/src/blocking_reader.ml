open Core
open Sexplib

module Simple_sexp_loader = struct
  module Monad = struct
    type 'a t = 'a

    let return a = a

    module Monad_infix = struct
      let ( >>= ) a f = f a
    end

    module List = List
  end

  let load_sexps file =
    try Sexp.load_sexps file with
    | e -> raise (Macro.add_error_location file e)
  ;;

  let load_annotated_sexps file =
    try Sexp.Annotated.load_sexps file with
    | e -> raise (Macro.add_error_location file e)
  ;;
end

module Simple_loader = Macro.Loader (Simple_sexp_loader)

let id a = a
let load_sexp_conv = Simple_loader.load_sexp_conv

let load_sexp_conv_exn ?allow_includes file f =
  match load_sexp_conv ?allow_includes file f with
  | `Result a -> a
  | `Error (exn, _) -> raise exn
;;

let load_sexp ?allow_includes file = load_sexp_conv_exn ?allow_includes file id
let load_sexps_conv = Simple_loader.load_sexps_conv

let load_sexps_conv_exn ?allow_includes file f =
  let results = load_sexps_conv ?allow_includes file f in
  List.map results ~f:(function
    | `Error (exn, _) -> raise exn
    | `Result a -> a)
;;

let load_sexps ?allow_includes file = load_sexps_conv_exn ?allow_includes file id
let included_files = Simple_loader.included_files
