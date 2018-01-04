open! Core
open Async

module type S = sig
  type t [@@deriving of_sexp]
end

let of_load ~name load =
  let open Command.Let_syntax in
  Command.async_or_error ~summary:(sprintf "Validate %s files" name)
    [%map_open
      let files = anon (sequence ("FILES" %: file)) in
      fun () ->
        Deferred.Or_error.List.iter files ~f:(fun filename ->
          load filename
          >>| Or_error.ignore)
    ]
;;

let create ?(multiple_sexps_per_file=false) ~name (module S : S) : Command.t =
  let load filename =
    if multiple_sexps_per_file then
      Reader.load_sexps filename S.t_of_sexp
      >>| Or_error.ignore
    else
      Reader.load_sexp filename S.t_of_sexp
      >>| Or_error.ignore
  in
  of_load ~name load
;;
