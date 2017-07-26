open Core
open Typerep_extended.Std

(* this can be dynamically changed via a config for example. What this means is that this
   program will understand any version <= max version, but downcast its output to v4 *)
let version = Type_struct.Versioned.Version.v4

type t = T : 'a Typerep.t -> t

let table = String.Table.create ()

let register (module A : Typerepable.S0) =
  let key = Typename.name A.typename_of_t in
  String.Table.set table ~key ~data:(T A.typerep_of_t)

let summary = "Get typerep metadata about registered protocols"
let readme () = "\
This is a simple toy command meant to be used as a demo.
It allows one to retrieve the typereps of some types used bin_prot exchanges.
"

module Elt = struct
  type t = [ `Not_defined | `Defined of Type_struct.Versioned.t ] [@@deriving sexp]
  let or_not_defined = function
    | None -> `Not_defined
    | Some t -> t
end

module Protocols = struct
  type t = (string * Elt.t) list
  [@@deriving sexp]
end

let multi =
  List.concat_map ~f:(fun s -> List.rev_map (String.split ~on:',' s) ~f:String.strip)

let find key =
  match String.Table.find table key with
  | None -> `Not_defined
  | Some (T typerep) ->
    let type_struct = Type_struct.Versioned.of_typerep ~version typerep in
    `Defined type_struct

let main
    types
    ()
    =
  let types = match types with [] -> String.Table.keys table | _ -> types in
  let types = List.sort types ~cmp:String.compare in
  let output = List.map types ~f:(fun key -> key, find key) in
  print_endline (Sexp.to_string (Protocols.sexp_of_t output))

let simple_command =
  let open Command.Spec in
  let types () = map ~f:multi (flag
    "-t"
    (listed string)
    ~doc:"<typename,typename2> specify only a few types to print. \
The default behavior is to get them all"
  )
  in
  Command.basic ~summary ~readme (
    empty
    +> types ()
  ) main
