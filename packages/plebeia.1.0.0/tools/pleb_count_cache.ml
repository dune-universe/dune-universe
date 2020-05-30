(*
   
   Show the hashcons cache status

   count_cache ~/.tezos-node/plebeia.context
*)

open Plebeia.Internal

let (//) = Filename.concat

let () =
  let path = Sys.argv.(1) in
  let vc = Vc.open_ ~mode:Storage.Reader path in
  let ctxt = Vc.context vc in
  let hashcons = ctxt.Context.hashcons in
  Hashcons.stat Format.err_formatter hashcons
