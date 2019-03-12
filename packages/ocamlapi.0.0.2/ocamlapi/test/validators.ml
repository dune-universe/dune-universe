open Core
open Mock

let validate_code expected (req, _body) =
    let actual = Cohttp.Response.status req
                 |> Cohttp.Code.code_of_status in
    let msg = sprintf "Expected status code %d but got status code %d" expected actual in
    OUnit.assert_equal ~msg:msg actual expected

let validate_body expected (_req, body) =
    let actual = Body.to_string body in
    let msg = sprintf "Expected body %s but got body %s" expected actual in
    OUnit.assert_equal ~msg:msg actual expected

let dispatch_and_validate dispatch_fun router req body validator =
    dispatch_fun router req body
    |> Io.run_synchronously
    |> validator
