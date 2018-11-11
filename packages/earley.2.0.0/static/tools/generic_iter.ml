open Asttypes
open Parsetree
open Longident

(* Generic functions *)
let iter_option fn o1 =
  match o1 with
  | None   -> ()
  | Some e1-> fn e1

let iter_list = List.iter

let do_local_ident = ref (fun _ -> ())

let rec iter_longident i1 =
  match i1 with
  | Lident s1 -> !do_local_ident s1
  | _ -> ()
