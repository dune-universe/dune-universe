open OUnit2
open Lilac
open Base

let ae exp got _test_ctxt =
  assert_equal exp got ~printer:(fun x -> "|" ^ x ^ "|")

let ae_option exp got _test_ctxt = 
  match exp, got with
  | Some e, Some g -> ae e g _test_ctxt
  | None, Some g -> ae "None" g _test_ctxt
  | Some e, None -> ae e "None" _test_ctxt
  | None, None -> ae "None" "None" _test_ctxt

let yaml =
  let y = yaml_from_fpath "res/config.yaml" in
  fun () ->
    y

let expected_config = "lilac-params:
  source:
    url: https://ttaw.dev
    user: lilac+source@ttaw.dev
    cred: ${LILAC_SOURCE_CRED}
  dest:
    url: https://walkandtalk.dev
    user: lilac+dest@walkandtalk.dev
    cred: ${LILAC_DEST_CRED}"

let fail_read path = 
  try
    Some (yaml_from_fpath path |> to_string_trim)
  with 
    _ -> None

let lilac_test = [
  "Invite" >:: ae "Come on. I'll open the wall for you." invite;

  "Look at the stars" >:: ae "\n\n\n         ✧\n       ✧\n                    ✧\n  ✧\n     ✧\n          ✧\n  ✧\n          ✧\n                   ✧\n           ✧\n   ✧\n                ✧\n                 ✧\n\n\n\n\n\n\n\n\n" constellation;

  "Source url" >:: ae_option (Some "https://ttaw.dev") (yaml () |> yaml_value_str ~path:"lilac-params.source.url");

  "Read config succeeds" >:: ae_option (Some expected_config) (Some(yaml () |> to_string_trim ));

  "Read config fails" >:: ae_option None (fail_read "./not-there.yaml");

  "Source user" >:: ae_option 
    (Some "lilac+source@ttaw.dev")
    (yaml () |> yaml_value_str ~path:"lilac-params.source.user")
    ;

  "Source cred" >:: ae_option 
    (Some "${LILAC_SOURCE_CRED}")
    (yaml () |> yaml_value_str ~path:"lilac-params.source.cred")
    ;

  "Dest url" >:: ae_option 
    (Some "https://walkandtalk.dev")
    (yaml () |> yaml_value_str ~path:"lilac-params.dest.url")
    ;

  "Dest user" >:: ae_option 
    (Some "lilac+dest@walkandtalk.dev")
    (yaml () |> yaml_value_str ~path:"lilac-params.dest.user")
    ;

  "Dest cred" >:: ae_option 
    (Some "${LILAC_DEST_CRED}")
    (yaml () |> yaml_value_str ~path:"lilac-params.dest.cred")
    ;

  "Not present source value" >:: ae_option 
    None (yaml () |> yaml_value_str ~path:"lilac-params.source.not-present");

  "Not present dest value" >:: ae_option 
    None (yaml () |> yaml_value_str ~path:"lilac-params.dest.not-present");

  "Not present lilac-params value" >:: ae_option 
    None (yaml () |> yaml_value_str ~path:"lilac-params.not-present");
  
  "Retreives sinlge element" >:: ae_option 
    (Some "1234") (retrieve ~keys:["c"] (Yaml.of_string "c: 1234" |> Result.ok));

  "Retreive empty results in None" >:: ae_option 
    None (retrieve ~keys:[] (Yaml.of_string "c: 1234" |> Result.ok));

  "key not found" >:: ae_option 
    None (yaml_get_key ~k:"" (Yaml.of_string "c: 1234" |> Result.ok) |> Option.map ~f:to_string_trim);

  "key not found" >:: ae_option 
    None (yaml_get_key ~k:"" (Yaml.of_string "null" |> Result.ok) |> Option.map ~f:to_string_trim);

]

let () =
  run_test_tt_main ("Lilac tests" >::: lilac_test)