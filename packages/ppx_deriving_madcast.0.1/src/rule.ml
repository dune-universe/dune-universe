
open Parsetree

type t =
  { name : string ;
    matcher : (core_type * core_type) -> (core_type * core_type) list option ;
    builder : expression list -> expression }

let make ~name ~matcher ~builder () =
  { name ; matcher ; builder }

let name_ rule = rule.name
let match_ rule = rule.matcher
let build_ rule = rule.builder
