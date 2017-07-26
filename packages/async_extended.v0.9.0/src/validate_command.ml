open! Core
open Async
open Command.Let_syntax

module type S = sig
  type t [@@deriving of_sexp]
end

let create ?(multiple_sexps_per_file=false) ~name s : Command.t =
  Command.async_or_error' ~summary:(sprintf "Validate %s files" name)
    [%map_open
      let files = anon (sequence ("FILES" %: file)) in
      fun () ->
        let module S = (val s : S) in
        Deferred.Or_error.List.iter files ~f:(fun file ->
          if multiple_sexps_per_file then
            Reader.load_sexps file S.t_of_sexp
            >>| Or_error.ignore
          else
            Reader.load_sexp file S.t_of_sexp
            >>| Or_error.ignore)
    ]
;;
