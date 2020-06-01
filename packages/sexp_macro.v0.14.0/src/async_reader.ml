open Core
open Async

type ('sexp, 'a, 'b) load =
  ?allow_includes:bool -> string -> ('sexp -> 'a) -> 'b Deferred.t

module Macro_loader = Macro.Loader (struct
    module Monad = struct
      type 'a t = 'a Deferred.t

      let return = return

      module Monad_infix = Deferred.Monad_infix

      module List = struct
        let iter xs ~f = Deferred.List.iter xs ~f
        let map xs ~f = Deferred.List.map xs ~f
      end
    end

    let load_sexps file =
      match%map
        Monitor.try_with ~extract_exn:true (fun () ->
          Reader.with_file file ~f:(fun t -> Pipe.to_list (Reader.read_sexps t)))
      with
      | Ok sexps -> sexps
      | Error e -> raise (Macro.add_error_location file e)
    ;;

    let load_annotated_sexps file =
      match%map
        Monitor.try_with ~extract_exn:true (fun () ->
          Reader.with_file file ~f:(fun t ->
            Pipe.to_list (Reader.read_annotated_sexps t)))
      with
      | Ok sexps -> sexps
      | Error e -> raise (Macro.add_error_location file e)
    ;;
  end)

let get_load_result_exn = function
  | `Result x -> x
  | `Error (exn, _sexp) -> raise exn
;;

let gen_load_sexp_exn (type a) ?allow_includes ~file ~(a_of_sexp : Sexp.t -> a) =
  Macro_loader.load_sexp_conv ?allow_includes file a_of_sexp >>| get_load_result_exn
;;

let load_sexp_exn ?allow_includes file a_of_sexp =
  gen_load_sexp_exn ?allow_includes ~file ~a_of_sexp
;;

let gen_load_sexp ?allow_includes ~file ~a_of_sexp =
  Deferred.Or_error.try_with ~extract_exn:true (fun () ->
    gen_load_sexp_exn ?allow_includes ~file ~a_of_sexp)
;;

let load_sexp ?allow_includes file a_of_sexp =
  gen_load_sexp ?allow_includes ~file ~a_of_sexp
;;

let gen_load_sexps_exn (type a) ?allow_includes ~file ~(a_of_sexp : Sexp.t -> a) =
  Macro_loader.load_sexps_conv ?allow_includes file a_of_sexp
  >>| List.map ~f:get_load_result_exn
;;

let load_sexps_exn ?allow_includes file a_of_sexp =
  gen_load_sexps_exn ?allow_includes ~file ~a_of_sexp
;;

let gen_load_sexps ?allow_includes ~file ~a_of_sexp =
  Deferred.Or_error.try_with ~extract_exn:true (fun () ->
    gen_load_sexps_exn ?allow_includes ~file ~a_of_sexp)
;;

let load_sexps ?allow_includes file a_of_sexp =
  gen_load_sexps ?allow_includes ~file ~a_of_sexp
;;

let included_files file =
  Deferred.Or_error.try_with (fun () -> Macro_loader.included_files file)
;;
