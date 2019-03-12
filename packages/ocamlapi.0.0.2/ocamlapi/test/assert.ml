open Core
open Mock.Ocamlapi

let route_args_equal =
    String.Map.equal String.equal
