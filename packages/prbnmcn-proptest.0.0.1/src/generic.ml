module type Name = sig
  val name : string
end

module type Generator = sig
  type t

  val gen : t Crowbar.gen

  val ( = ) : t -> t -> bool
end

(* Helpers *)
let add_test name bool_gen = Crowbar.add_test ~name [bool_gen] @@ Crowbar.check

let apply gen k =
  let open Crowbar in
  map [gen] k

let apply_pair gen k =
  let open Crowbar in
  map [gen; gen] k

let apply_triple gen k =
  let open Crowbar in
  map [gen; gen; gen] k

let sf = Printf.sprintf
