open Core
open OUnit
open Cohttp
open Mock

let name = "Route tests"

let failure_callback msg =
    fun _ ->
        assert_failure msg

let variable_set lst =
    match String.Table.of_alist lst with
    | `Duplicate_key key -> failwith (sprintf "Duplicate key in test input %s" key)
    | `Ok set -> set

(* Ignore all input and return "Success" as response body *)
let simple_success_callback ?vars:_ _ _ =
    respond_success_string "Success"

let validate_vars_callback expected ?vars:(vars=String.Table.create ()) _ _ =
    assert_equal ~cmp:(fun t1 t2 -> String.Table.equal t1 t2 String.equal) expected vars;
    respond_success_string "Success"

let dummy_host = "http://localhost:8080"

let localhost path =
    dummy_host ^ path |> Uri.of_string 

let dummy_path = "/foo/bar/baz"

let dummy_path_with_variable = "/foo/bar/<baz>"

let all_variables = "/<one>/<two>/<three>"

let tests = [

    "Simple route test" >::
    (fun _ ->
        let route = dummy_path, [ `GET, simple_success_callback ] in

        let router = Router.create_exn [ route ] in

        let req = Request.make ~meth:`GET (localhost dummy_path) in
        let body = Body.empty in
        Validators.dispatch_and_validate
            Router.dispatch
            router
            req
            body
            (Validators.validate_code 200)
    );

    "Route test with variable" >::
    (fun _ ->
        let route = dummy_path_with_variable, [ `GET, simple_success_callback ] in

        let router = Router.create_exn [ route ] in

        let req = Request.make ~meth:`GET (localhost "/foo/bar/flub") in
        Validators.dispatch_and_validate
            Router.dispatch
            router
            req
            Body.empty
            (Validators.validate_code 200)
    );

    "Route test with all variables" >::
    (fun _ ->

        let validate_vars = [ "one", "val1"; "two", "val2"; "three", "val3" ]
                            |> variable_set
                            |> validate_vars_callback in

        let route = all_variables, [ `GET, validate_vars ] in

        let router = Router.create_exn [ route ] in
        let req = Request.make ~meth:`GET (localhost "/val1/val2/val3") in
        Validators.dispatch_and_validate
            Router.dispatch
            router req
            Body.empty
            (Validators.validate_code 200)
    );

    "Test if variables are respected with otherwise same route" >::
    (fun _ ->
        let validate_vars_route = [ "bar", "flub" ]
                                  |> variable_set
                                  |> validate_vars_callback in
        let validate_no_vars_route = String.Table.create () |> validate_vars_callback in
        let route1 = "/foo/<bar>/baz", [ `GET, validate_vars_route ] in
        let route2 = "/foo/bar/baz", [ `GET, validate_no_vars_route ] in

        let router = Router.create_exn [ route1; route2 ] in

        let req1 = Request.make ~meth:`GET (localhost "/foo/flub/baz") in
        Validators.dispatch_and_validate Router.dispatch router req1 Body.empty
            (Validators.validate_code 200);

        let req2 = Request.make ~meth:`GET (localhost "/foo/bar/baz") in
        Validators.dispatch_and_validate
            Router.dispatch
            router
            req2
            Body.empty
            (Validators.validate_code 200)
    );

    "Test http method to callback mapping" >::
    (fun _ ->
        let route = "/foo/bar/baz", [ `GET, (fun ?vars:_ _ _ -> respond_success_string "GET response")
                                    ; `POST, (fun ?vars:_ _ _ -> respond_success_string "POST response") ] in

        let router = Router.create_exn [ route ] in 

        let post_req = Request.make ~meth:`POST (localhost "/foo/bar/baz") in
        Validators.dispatch_and_validate
            Router.dispatch
            router
            post_req
            Body.empty
            (Validators.validate_body "POST response");

        let get_req = Request.make ~meth:`GET (localhost "/foo/bar/baz") in
        Validators.dispatch_and_validate
            Router.dispatch
            router
            get_req
            Body.empty 
            (Validators.validate_body "GET response")
    );

    "Test trailing slash" >::
    (fun _ ->
        let route = "/foo/bar/baz", [ `GET, (fun ?vars:_ _ _ -> respond_success_string "No trailing slash")] in
        let route' = "/foo/bar/baz/", [ `GET, (fun ?vars:_ _ _ -> respond_success_string "Trailing slash")] in
        
        let router = Router.create_exn [ route; route' ] in
        
        let get_req = Request.make ~meth: `GET (localhost "/foo/bar/baz") in
        Validators.dispatch_and_validate
            Router.dispatch
            router
            get_req
            Body.empty
            (Validators.validate_body "No trailing slash");
        let get_req' = Request.make ~meth: `GET (localhost "/foo/bar/baz/") in
        Validators.dispatch_and_validate
            Router.dispatch
            router
            get_req'
            Body.empty
            (Validators.validate_body "Trailing slash")
    );

    "Test paths that are prefixes of one another" >::
    (fun _ ->
        let route = "/foo/bar/baz", [ `GET, (fun ?vars:_ _ _ -> respond_success_string "Full path") ] in
        let route' = "/foo/bar", [ `GET, (fun ?vars:_ _ _ -> respond_success_string "Prefix") ] in
        
        let router = Router.create_exn [ route; route' ] in

        let get_req = Request.make ~meth:`GET (localhost "/foo/bar") in
        Validators.dispatch_and_validate
            Router.dispatch
            router
            get_req
            Body.empty
            (Validators.validate_body "Prefix");
        let get_req = Request.make ~meth:`GET (localhost "/foo/bar/baz") in
        Validators.dispatch_and_validate
            Router.dispatch
            router
            get_req
            Body.empty
            (Validators.validate_body "Full path")
    );

    "Test precedence of static path over variable" >::
    (fun _ ->
        let route = "/foo/<bar>/baz", [ `GET, (fun ?vars:_ _ _ -> respond_success_string "Path with variable") ] in
        let route' = "/foo/bar", [ `GET, (fun ?vars:_ _ _ -> respond_success_string "Path without variable") ] in
        
        let router = Router.create_exn [ route; route' ] in

        let get_req = Request.make ~meth:`GET (localhost "/foo/bar/baz") in
        Validators.dispatch_and_validate
            Router.dispatch
            router
            get_req
            Body.empty
            (Validators.validate_code 404);
        let get_req = Request.make ~meth:`GET (localhost "/foo/foo/baz") in
        Validators.dispatch_and_validate
            Router.dispatch
            router
            get_req
            Body.empty
            (Validators.validate_body "Path with variable")
    
    );

    "Test custom exception handler and fallback is used" >::
    (fun _ ->
        let route = "/foo", [ `GET, (fun ?vars:_ _ _ -> raise (Mock.Exceptions.Bad_request "Some bad request")) ] in
       
        (* Define a module with an exception handler and fallback response function. *)
        let module Custom_config = struct

            let default_exn_handler ?vars:_ exn =
                let status, body =
                match exn with
                | Exceptions.Bad_request _msg -> `Bad_request, Body.empty
                | _ -> `Internal_server_error, Body.of_string "An unexpected error occurred" in
                Io.return (Response.make ~status (), body)

            let default_fallback ?vars:_ _req _body =
            (* TODO: add an example of using a variable. *)
                Io.return (Response.make ~status:`Not_found (), Body.of_string "Resource not found")
        end in
        let module Router = Ocamlapi.Make_with_config (Cohttp.Request)
                                                             (Body)
                                                             (Cohttp.Response)
                                                             (Io)
                                                             (Custom_config) in
        let router = Router.create_exn [ route ] in
        
        let get_req = Request.make ~meth:`GET (localhost "/foo") in
        
        (* Make request to route whose callback throws exception, verify handler gets called. *)
        Validators.dispatch_and_validate
            Router.dispatch
            router
            get_req
            Body.empty
            (Validators.validate_code 400);
        (* Make request to nonexistent route, verify fallback gets called.*)
        let get_req = Request.make ~meth:`GET (localhost "/flub") in
        Validators.dispatch_and_validate
            Router.dispatch
            router
            get_req
            (Body.of_string "An unexpected error occurred")
            (fun resp ->
                Validators.validate_code 404 resp;
                Validators.validate_body "Resource not found" resp)
    )
]
