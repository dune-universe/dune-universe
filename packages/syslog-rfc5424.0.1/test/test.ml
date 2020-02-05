open Astring
open Rfc5424
open Alcotest

let sd_name_valid = [
  "ex1", "exampleSDID@32473" ;
  "ex2", "sigSig" ;
]

let structured_data_valid = [
  "empty", "-" ;
  "ex1", {|[exampleSDID@32473 iut="3" eventSource="Application" eventID="1011"]|} ;
  "ex2", {|[exampleSDID@32473 iut="3" eventSource="Application" eventID="1011"][examplePriority@32473 class="high"]|} ;
  "ex3", {|[sigSig ver="1" rsID="1234" signature="..."]|} ;
]

let structured_data_invalid = [
  "ex1", {|[exampleSDID@32473 iut="3" eventSource="Application" eventID="1011"] [examplePriority@32473 class="high"]|} ;
  "ex2", {|[ exampleSDID@32473 iut="3" eventSource="Application" eventID="1011"][examplePriority@32473 class="high"]|} ;
]

let full_valid = [
  "ex1", {|<34>1 2003-10-11T22:14:15.003Z mymachine.example.com su - ID47 - BOM'su root' failed for lonvick on /dev/pts/8|} ;
  "ex2", {|<165>1 2003-08-24T05:14:15.000003-07:00 192.0.2.1 myproc 8710 - - %% It's time to make the do-nuts.|} ;
  "ex3", {|<165>1 2003-10-11T22:14:15.003Z mymachine.example.com evntslog - ID47 [exampleSDID@32473 iut="3" eventSource="Application" eventID="1011"] BOMAn application event log entry...|} ;
  "ex4", {|<165>1 2003-10-11T22:14:15.003Z mymachine.example.com evntslog - ID47 [exampleSDID@32473 iut="3" eventSource="Application" eventID="1011"][examplePriority@32473 class="high"]|} ;
  "ex5", {|<14>1 2019-01-19T19:43:51.470399Z haramis titania.test.dummy_worker(1) 8348 - [ovh X-OVH-TOKEN="b5f5fb79-301a-4e9b-a413-12b788f6534e"][logs ] BOMWorker started for worker_init|} ;
]

let full_invalid = [
]

let trip = [
  "minimal", Rfc5424.create ~ts:Ptime.epoch () ;
  "empty_hostname", Rfc5424.create ~ts:Ptime.epoch ~hostname:"" () ;
  "empty", Rfc5424.create ~ts:Ptime.epoch () ;
  "empty_ascii", Rfc5424.create ~ts:Ptime.epoch ~msg:(`Ascii "") () ;
  "empty_utf8", Rfc5424.create ~ts:Ptime.epoch ~msg:(`Utf8 "") () ;
]

let structured_data =
  testable pp_print_structured_data equal_structured_data
let syslog = testable Rfc5424.pp Rfc5424.equal

let string_ts =
  let equal a b =
    let suma = String.fold_left (fun a c -> a + Char.to_int c) 0 a in
    let sumb = String.fold_left (fun a c -> a + Char.to_int c) 0 b in
    String.length a = String.length b && abs (suma - sumb) < 2 in
  testable Format.pp_print_string equal

let parse_print_structured_data =
  let re = Tyre.(compile (whole_string Rfc5424.structured_data)) in
  fun ?(valid=true) s ->
    if not valid then
      match Tyre.exec re s with
      | Error _ -> ()
      | Ok a -> failf "Found %d data, should have failed" (List.length a)
    else
      match Tyre.exec re s with
      | Error e -> failf "%a" Tyre.pp_error e
      | Ok data ->
        let s' = Format.asprintf "%a" pp_print_structured_data data in
        match Tyre.exec re s' with
        | Error e -> failf "%a" Tyre.pp_error e
        | Ok data' ->
          check string "string equality" s s' ;
          check structured_data "type equality" data data'

let roundtrip t =
  (* let r = Rfc5424_capnp.capnp_of_syslog t in
   * let rs = Rfc5424_capnp.syslog_of_capnp r in *)
  let s' = Format.asprintf "%a" Rfc5424.pp t in
  (* let rs' = Format.asprintf "%a" Rfc5424.pp rs in *)
  (* check string_ts "capnp string equality" s' rs' ; *)
  match of_string s' with
  | Error e -> failf "%a" Tyre.pp_error e
  | Ok t' ->
    let s'' = Format.asprintf "%a" Rfc5424.pp t' in
    check syslog "type equality" t t' ;
    check string "string equality" s' s''

let parse_print_full =
  fun ?(valid=true) s ->
  if not valid then
    match of_string s with
    | Error _ -> ()
    | Ok t -> failf "Found %a, should have failed" Rfc5424.pp t
  else
    match of_string s with
    | Error e -> failf "%a" Tyre.pp_error e
    | Ok t -> roundtrip t

let parse_print_sd_name =
  let re = Tyre.compile sd_name in
  fun s ->
    match Tyre.exec re s with
    | Ok data ->
      check string "" s data
    | Error e -> failf "%a" Tyre.pp_error e

let sd_name_valid =
  "sd_name_valid",
   List.map begin fun (n, s) ->
     test_case n `Quick (fun () -> parse_print_sd_name s)
   end sd_name_valid

let structured_data_valid =
  "structured_data_valid",
   List.map begin fun (n, s) ->
     test_case n `Quick (fun () -> parse_print_structured_data s)
   end structured_data_valid

let structured_data_invalid =
  "structured_data_invalid",
   List.map begin fun (n, s) ->
     test_case n `Quick (fun () -> parse_print_structured_data ~valid:false s)
   end structured_data_invalid

let trip =
  "trip",
  List.map begin fun (n, t) ->
     test_case n `Quick (fun () -> roundtrip t)
  end trip

let full_valid =
  "full_valid",
   List.map begin fun (n, s) ->
     test_case n `Quick (fun () -> parse_print_full s)
   end full_valid

let full_invalid =
  "full_invalid",
   List.map begin fun (n, s) ->
     test_case n `Quick (fun () -> parse_print_full ~valid:false s)
   end full_invalid

let () =
  run "rfc5424" [
    sd_name_valid ;
    structured_data_valid ;
    structured_data_invalid ;
    trip ;
    full_valid ;
    full_invalid ;
  ]
