open Core
open Async
include Delimited_kernel.Shared

let drop_lines r lines =
  let rec loop n =
    if n = 0
    then Deferred.unit
    else (
      match%bind Reader.read_line r with
      | `Ok _ -> loop (n - 1)
      | `Eof -> failwithf "file has fewer than %i lines" lines ())
  in
  loop lines
;;
