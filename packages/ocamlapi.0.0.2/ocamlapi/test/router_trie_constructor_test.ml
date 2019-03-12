open Core
open OUnit
open Mock
open Ocamlapi.Route_exceptions

let name = "Router trie constructor tests"

(*TODO: move all such duplicated helpers to their own file *)
let simple_success_callback ?vars:_ _ _ =
    respond_success_string "Success"

let tests = [
    "Test duplicate static paths causes error" >::
    (fun _ ->
        let route = "/foo/bar/baz", [ `GET, simple_success_callback ] in
        Router.create [ route; route ]
        |> function
           | Ok _ -> assert_failure "Router was created from duplicate paths"
           | Error (DuplicateRouteTemplate "/foo/bar/baz") -> ()
           | Error x -> raise x (* Wrong error *));

    "Test duplicate variable paths causes error" >::
    (fun _ ->
        let route = "/foo/bar/<baz>", [ `GET, simple_success_callback ] in
        let route' = "/foo/bar/<flub>", [ `GET, simple_success_callback ] in
        Router.create [ route; route' ]
        |> function
           | Ok _ -> assert_failure "Router was created from duplicate paths with different variable names"
           | Error (DuplicateRouteTemplate "/foo/bar/<flub>") -> ()
           | Error x -> raise x);

    "Test malformed template causes error" >::
    (fun _ ->
        let route = "/foo/bar/<baz", [ `GET, simple_success_callback ] in
        Router.create [ route ]
        |> function
           | Ok _ -> assert_failure "Router was created from malformed url template"
           | Error (InvalidRouteTemplate _) -> ()
           | Error x -> raise x);

    "Test duplicate variable in template causes error" >::
    (fun _ ->
        let route  = "/foo/<bar>/foo/<bar>", [ `GET, simple_success_callback ] in
        Router.create [ route ]
        |> function
           | Ok _ -> assert_failure "Router was created from a url template with duplicate variable names"
           | Error _ -> ())
]
