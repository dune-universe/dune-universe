open Bgp
open Alcotest
open Operators

open Bgp_cstruct

module Prefix = Ipaddr.V4.Prefix

let set_bit n pos b =
  if (n > 255) then raise (Failure "Invalid argument: n is too large.")
  else if (pos > 7) then raise (Failure "Invalid argument: pos is too large.")
  else
    let n_32 = Int32.of_int n in 
    let res_32 = 
      match b with
      | 0 -> (n_32 ^^^ (1_l <<< pos))
      | 1 -> (n_32 ||| (1_l <<< pos))
      | _ -> raise (Failure "Invalid argument: b should be either 0 or 1.")
    in
      Int32.to_int res_32
;;

let attr_flags_to_int {optional; transitive; partial; extlen} =
  let n_ref = ref 0 in
  if (optional) then n_ref := set_bit (!n_ref) 7 1;
  if (transitive) then n_ref := set_bit (!n_ref) 6 1;
  if (partial) then n_ref := set_bit (!n_ref) 5 1;
  if (extlen) then n_ref := set_bit (!n_ref) 4 1;
  !n_ref
;;

let test_find_origin () =
  let path_attrs = [
    Origin EGP;
    As_path [Asn_seq [1_l]];
  ] in
  assert (find_origin path_attrs = EGP);

  let tmp = set_origin path_attrs IGP in
  assert (find_origin tmp = IGP);
  assert (tmp = [Origin IGP; As_path [Asn_seq [1_l]];]);
;;

let test_find_aspath () =
  let path_attrs = [
    Origin EGP;
    As_path [Asn_seq [1_l]];
  ] in
  assert (find_as_path path_attrs = [Asn_seq [1_l]]);

  let p2 = set_as_path path_attrs [Asn_seq [1_l; 2_l]] in
  assert (find_as_path p2 = [Asn_seq [1_l; 2_l]]);
  assert (p2 = [ Origin EGP; As_path [Asn_seq [1_l; 2_l]];]);
;;

let test_find_next_hop () =
  let id = Ipaddr.V4.of_string_exn "172.19.10.1" in
  let path_attrs = [
    Origin EGP;
    As_path [Asn_seq [1_l]];
    Next_hop id;
  ] in
  assert (find_next_hop path_attrs = id);

  let id2 = Ipaddr.V4.of_string_exn "172.19.10.2" in
  let p2 = set_next_hop path_attrs id2 in
  assert (find_next_hop p2 = id2);

  let p3 = [
    Origin EGP;
    As_path [Asn_seq [1_l]];
    Next_hop id2;
  ] in
  assert (p2 = p3)
;;

let test_find_med () =
  let path_attrs = [
    Origin EGP;
    As_path [Asn_seq [1_l]];
    Next_hop (Ipaddr.V4.of_string_exn "172.19.10.1");
  ] in
  assert (find_med path_attrs = None);

  let p2 = set_med path_attrs (Some 10_l) in
  assert (find_med p2 = Some 10_l);

  let p3 = set_med p2 None in
  assert (find_med p3 = None);
;;
  
let test_find_local_pref () =
  let path_attrs = [
    Origin EGP;
    As_path [Asn_seq [1_l]];
    Next_hop (Ipaddr.V4.of_string_exn "172.19.10.1");
  ] in
  assert (find_local_pref path_attrs = None);

  let p2 = set_local_pref path_attrs (Some 10_l) in
  assert (find_local_pref p2 = Some 10_l);

  let p3 = set_local_pref p2 None in
  assert (find_local_pref p3 = None);
;;


let test_path_attrs_mem () =
  let path_attrs = [
    ( Origin EGP);
    ( As_path [Asn_seq [1_l]]);
  ] in
  assert (path_attrs_mem ORIGIN path_attrs);
  assert (path_attrs_mem AS_PATH path_attrs);
  assert (path_attrs_mem NEXT_HOP path_attrs = false);
;;

let test_path_attrs_remove () =

  let path_attrs = [
    ( Origin EGP);
    ( As_path [Asn_seq [1_l]]);
  ] in

  let tmp = path_attrs_remove ORIGIN path_attrs in

  assert (List.length tmp = 1);
  assert (path_attrs_mem ORIGIN tmp = false);
;;

let test_parse_gen_combo t =
  let msg1 = gen_msg t in
  let t2 = 
    match (parse_buffer_to_t msg1) with 
    | Error err -> 
      failwith (parse_error_to_string err)
    | Ok v -> v 
  in
  assert (Bgp.equal t t2);
  
  Printf.printf "Test pass: %s\n" (Bgp.to_string t2)
;;

let test_parse_exn buf wanted_err =
  parse_buffer_to_t buf |> function
    | Ok _ -> fail "This should give an exception."
    | Error e -> assert (e = wanted_err)
;;

let test_normal_update =
  let f () =
    let withdrawn = [
      (Prefix.make 16 (Ipaddr.V4.of_string_exn "192.168.0.0")); 
      (Prefix.make 8 (Ipaddr.V4.of_string_exn "10.0.0.0"));
      (Prefix.make 24 (Ipaddr.V4.of_string_exn "172.16.84.0"));  
    ] in
    let nlri = [
      (Prefix.make 16 (Ipaddr.V4.of_string_exn "192.169.0.0")); 
    ] in

    let path_attrs = [
       Origin IGP;
       As_path [Asn_set [2_l; 5_l; 3_l]; Asn_seq [10_l; 20_l; 30_l]];
       Next_hop (Ipaddr.V4.of_string_exn "192.168.1.253");
    ] in 
    let u = Update {withdrawn; path_attrs; nlri} in
    test_parse_gen_combo u
  in
  test_case 
    "Naive test for parsing and generation of update messages"
    `Slow f 
;;

let test_update_only_withdrawn =
  let f () =
    let withdrawn = [
      (Prefix.make 16 (Ipaddr.V4.of_string_exn "192.168.0.0")); 
      (Prefix.make 8 (Ipaddr.V4.of_string_exn "10.0.0.0"));
      (Prefix.make 24 (Ipaddr.V4.of_string_exn "172.16.84.0"));  
    ] in  
    let u = Update {withdrawn; path_attrs=[]; nlri=[]} in
    test_parse_gen_combo u
  in
  test_case 
    "test_update_only_withdrawn"
    `Slow f 
;;

let test_update_only_nlri =
  let f () =
    let nlri = [
      (Prefix.make 16 (Ipaddr.V4.of_string_exn "192.169.0.0")); 
    ] in

    let path_attrs = [
      Origin IGP;
      As_path [Asn_set [2_l; 5_l; 3_l]; Asn_seq [10_l; 20_l; 30_l]];
      Next_hop (Ipaddr.V4.of_string_exn "192.168.1.253");
      Med (Int32.of_int 10);
      Local_pref (Int32.of_int 20);
      Atomic_aggr;
    ] in 
    let u = Update {withdrawn = []; path_attrs; nlri} in
    test_parse_gen_combo u
  in
  test_case 
    "test_update_only_nlri"
    `Slow f
;;

let test_update_with_unknown_attr () =
  let flags = {
    transitive = true;
    optional = true;
    partial = false;
    extlen = false;
  } in

  let nlri = [
    (Prefix.make 16 (Ipaddr.V4.of_string_exn "192.169.0.0")); 
  ] in
  let path_attrs = [
    Origin IGP;
    Origin IGP;
    As_path [Asn_set [2_l; 5_l; 3_l]; Asn_seq [10_l; 20_l; 30_l]];
    Next_hop (Ipaddr.V4.of_string_exn "192.168.1.253");
  ] in 
  let buf = gen_msg (Update {withdrawn = []; path_attrs; nlri}) in

  (* Modify the type code to something unknown *)
  Cstruct.set_uint8 buf 23 (attr_flags_to_int flags);
  Cstruct.set_uint8 buf 24 300;

  match parse_buffer_to_t buf with
  | Ok (Update { path_attrs } as msg) ->
    assert (List.length path_attrs = 4);
    test_parse_gen_combo msg
  | Ok _ -> 
    assert false
  | Error err -> 
    Printf.printf "%s\n" (parse_error_to_string err);
    assert false
;;

let test_open =
  let f () =
    let o = {
      version=4;
      local_asn = 2_l;
      hold_time=180;
      local_id = Ipaddr.V4.of_string_exn "172.19.0.3";
      options=[]
    } in
    test_parse_gen_combo (Open o)
  in 
  test_case "General test for open messages." `Quick f
;;

let test_notify =
  let f () = 
    let err = Message_header_error (Bad_message_length 50) in
    test_parse_gen_combo (Notification err)
  in 
  test_case "Simple test for notification" `Slow f
;;

let test_keepalive =
  let f () = test_parse_gen_combo Keepalive in
  test_case "Simple test for keepalive." `Slow f
;;

let test_len_pfxs_buffer () = 
  let pfxs = [
    (Prefix.make 16 (Ipaddr.V4.of_string_exn "192.168.0.0")); 
    (Prefix.make 8 (Ipaddr.V4.of_string_exn "10.0.0.0"));
    (Prefix.make 24 (Ipaddr.V4.of_string_exn "172.16.84.0")); 
  ] in
  assert (len_pfxs_buffer pfxs = 9)
;;

let test_len_path_attrs_buffer () = 
  let path_attrs = [
     Origin IGP;
     As_path [Asn_set [2_l; 5_l; 3_l]; Asn_seq [10_l; 20_l; 30_l]];
     Next_hop (Ipaddr.V4.of_string_exn "192.168.1.253");
  ] in 
  assert (len_path_attrs_buffer path_attrs = 4 + 19 + 7)
;;

let test_len_update_buffer () =
  let nlri = [Prefix.make 24 (Ipaddr.V4.of_string_exn "192.168.45.0")] in
  let path_attrs = [
     Origin IGP;
     As_path [Asn_set [2_l; 5_l; 3_l]; Asn_seq [10_l; 20_l; 30_l]];
     Next_hop (Ipaddr.V4.of_string_exn "192.168.1.253");
  ] in 
  let u = {withdrawn = []; path_attrs; nlri} in
  assert (len_update_buffer u = 23 + len_path_attrs_buffer path_attrs + len_pfxs_buffer nlri)
;;

let test_header_sync_error () =
  let buf = Cstruct.create 19 in
  Cstruct.BE.set_uint16 buf 16 19;
  Cstruct.set_uint8 buf 18 4;

  match parse_buffer_to_t buf with
  | Ok _ -> assert false
  | Error e -> 
    assert (e = (Msg_fmt_error (Parse_msg_h_err Connection_not_synchroniszed)))
;;

let test_header_bad_length_error () =
  let buf = Cstruct.create 19 in
  let marker, _ = Cstruct.split buf 16 in
  Cstruct.memset marker 0xff;
  Cstruct.BE.set_uint16 buf 16 19;
  Cstruct.set_uint8 buf 18 2;
  
  match parse_buffer_to_t buf with
  | Ok _ -> assert false
  | Error err ->
    assert (err = Msg_fmt_error (Parse_msg_h_err (Bad_message_length 19)))
;;

let test_header_bad_message_type () =
  let buf = Cstruct.create 19 in
  let marker, _ = Cstruct.split buf 16 in
  Cstruct.memset marker 0xff;
  Cstruct.BE.set_uint16 buf 16 19;
  set_h_typ buf 6;

  match parse_buffer_to_t buf with
  | Ok _ -> assert false
  | Error err ->
    assert (err = Msg_fmt_error (Parse_msg_h_err (Bad_message_type 6)))
;;

let test_update_duplicated_attr () =

  let path_attrs = [
    ( Origin EGP);
    ( As_path [Asn_seq [1_l]]);
    ( Origin EGP);
  ] in
  let nlri = [Prefix.make 24 (Ipaddr.V4.of_string_exn "192.168.45.0")] in
  let buf = gen_msg (Update { withdrawn = []; path_attrs; nlri }) in

  match parse_buffer_to_t buf with
  | Ok _ -> assert false
  | Error err ->
    assert (err = Msg_fmt_error (Parse_update_msg_err Malformed_attribute_list))
;;

let test_update_missing_well_known_attr () = 

  let path_attrs = [
    ( Origin EGP);
    ( As_path [Asn_seq [1_l]]);
  ] in
  let nlri = [Prefix.make 24 (Ipaddr.V4.of_string_exn "192.168.45.0")] in
  let buf = gen_msg (Update { withdrawn = []; path_attrs; nlri }) in

  match parse_buffer_to_t buf with
  | Ok _ -> assert false
  | Error err ->
    assert (err = Msg_fmt_error (Parse_update_msg_err (Missing_wellknown_attribute 3)))
;;



let test_update_attr_flags_err () = 
  let flags = {
    transitive = false;
    optional = true;
    partial = false;
    extlen = false;
  } in

  let path_attrs = [
    ( Origin EGP);
    ( As_path [Asn_seq [1_l]]);
     Next_hop (Ipaddr.V4.of_string_exn "192.168.1.253");
  ] in
  let nlri = [Prefix.make 24 (Ipaddr.V4.of_string_exn "192.168.45.0")] in
  let buf = gen_msg (Update { withdrawn = []; path_attrs; nlri }) in

  Cstruct.set_uint8 buf 23 (attr_flags_to_int flags);
  match parse_buffer_to_t buf with
  | Ok _ -> assert false
  | Error (Msg_fmt_error (Parse_update_msg_err (Attribute_flags_error _))) ->
    assert true
  | Error _ -> assert false
;;



let test_update_attr_length_err () = 
  let path_attrs = [
    ( Origin EGP);
    ( As_path [Asn_seq [1_l]]);
     Next_hop (Ipaddr.V4.of_string_exn "192.168.1.253");
  ] in
  let nlri = [Prefix.make 24 (Ipaddr.V4.of_string_exn "192.168.45.0")] in
  
  let buf = gen_msg (Update { withdrawn = []; path_attrs; nlri }) in

  Cstruct.set_uint8 buf 25 2;

  match parse_buffer_to_t buf with
  | Ok _ -> assert false
  | Error (Msg_fmt_error (Parse_update_msg_err (Attribute_length_error _))) ->
    assert true
  | Error err -> 
    Printf.printf "%s" (parse_error_to_string err);
    assert false
;;

let test_update_attr_unrecognized_wn_attr () =
  let flags = {
    transitive = false;
    optional = false;
    partial = false;
    extlen = false;
  } in

  let path_attrs = [
    Origin EGP;
    Origin EGP;
    As_path [Asn_seq [1_l]];
    Next_hop (Ipaddr.V4.of_string_exn "192.168.1.253");
  ] in
  let nlri = [Prefix.make 24 (Ipaddr.V4.of_string_exn "192.168.45.0")] in
  let buf = gen_msg (Update { withdrawn = []; path_attrs; nlri }) in

  Cstruct.set_uint8 buf 23 (attr_flags_to_int flags);
  Cstruct.set_uint8 buf 24 10;

  match parse_buffer_to_t buf with
  | Ok _ -> assert false
  | Error (Msg_fmt_error (Parse_update_msg_err (Unrecognized_wellknown_attribute _))) ->
    assert true
  | Error err -> 
    Printf.printf "%s" (parse_error_to_string err);
    assert false
;;

let test_update_invalid_origin () = 
  let path_attrs = [
    ( Origin EGP);
    ( As_path [Asn_seq [1_l]]);
    ( Next_hop (Ipaddr.V4.of_string_exn "192.168.1.253"));
  ] in
  let nlri = [Prefix.make 24 (Ipaddr.V4.of_string_exn "192.168.45.0")] in
  let buf = gen_msg (Update { withdrawn = []; path_attrs; nlri }) in

  Cstruct.set_uint8 buf 26 4;

  match parse_buffer_to_t buf with
  | Ok _ -> assert false
  | Error (Msg_fmt_error (Parse_update_msg_err (Invalid_origin_attribute _))) ->
    assert true
  | Error err -> 
    Printf.printf "%s" (parse_error_to_string err);
    assert false
;;

let test_path_attrs_to_string () =
  let path_attrs = [
    As_path [Asn_set [5_l; 1_l; 2_l]];
    Origin EGP;
    Next_hop (Ipaddr.V4.of_string_exn "192.168.1.253");
  ] in
  Printf.printf "%s\n" (path_attrs_to_string path_attrs)
;;

let test_parse_gen_capabilities () = 
  let o = {
    version=4;
    local_asn = 2_l;
    hold_time=180;
    local_id = Ipaddr.V4.of_string_exn "172.19.0.3";
    options= [ Capability [ Mp_ext(Afi.IP4, Safi.UNICAST)] ]
  } in
  test_parse_gen_combo (Open o);

  let o = {
    version=4;
    local_asn = 2_l;
    hold_time=180;
    local_id = Ipaddr.V4.of_string_exn "172.19.0.3";
    options= [ Capability [ Mp_ext(Afi.IP4, Safi.UNICAST); Route_refresh ] ]
  } in
  test_parse_gen_combo (Open o);

  let o = {
    version=4;
    local_asn = 2_l;
    hold_time=180;
    local_id = Ipaddr.V4.of_string_exn "172.19.0.3";
    options= [ Capability [Mp_ext(Afi.IP4, Safi.UNICAST)]; Capability [Route_refresh] ]
  } in
  test_parse_gen_combo (Open o);
;;

let test_parse_pattrs () =
  let path_attrs = [
    Origin EGP;
    Next_hop (Ipaddr.V4.of_string_exn "192.168.1.253");
    As_path [Asn_seq [1_l; 3_l;]; Asn_set [7_l; 3_l;]];
  ] in

  let path_attrs2 = [
    Origin EGP;
    As_path [Asn_seq [1_l; 3_l;]; Asn_set [3_l; 7_l]];
    Next_hop (Ipaddr.V4.of_string_exn "192.168.1.253");
  ] in

  let withdrawn = [] in
  let nlri = [ Prefix.make 24 (Ipaddr.V4.of_string_exn "172.16.84.0"); ] in
  
  let update1 = { withdrawn; path_attrs; nlri; } in
  let update2 = { withdrawn; path_attrs = path_attrs2; nlri; } in

  match parse_buffer_to_t @@ gen_update update1 with
  | Error _ -> assert false
  | Ok t -> assert (t = Update update2)
;;
  


let () =
  Printexc.record_backtrace true;
  run "bgp" [
    "util", [
      test_case "test find_origin" `Slow test_find_origin;
      test_case "test find_aspath" `Slow test_find_aspath;
      test_case "test find_next_hop" `Slow test_find_next_hop;
      test_case "test find_med" `Slow test_find_med;
      test_case "test find_local_pref" `Slow test_find_local_pref;
      test_case "test path_attrs_mem" `Slow test_path_attrs_mem;
      test_case "test path_attrs_remove" `Slow test_path_attrs_remove;
      test_case "test path_attrs_to_string" `Slow test_path_attrs_to_string;
    ];
    "update", [
      test_normal_update; 
      test_update_only_nlri; 
      test_update_only_withdrawn;
      test_case "test update_with_unknown_attr" `Slow test_update_with_unknown_attr;
      test_case "test parse_path_attrs" `Slow test_parse_pattrs;
    ];
    "open", [
      test_open;
      test_case "test parse and gen capabilities" `Slow test_parse_gen_capabilities;
    ];
    "keepalive", [test_keepalive];
    "notification", [test_notify];
    "len", [
      test_case "test len pfxs buffer" `Slow test_len_pfxs_buffer;
      test_case "test len path attrs buffer" `Slow test_len_path_attrs_buffer;
      test_case "test len update buffer" `Slow test_len_update_buffer;
    ];
    "error", [
      test_case "test error: connection_not_synchronized" `Slow test_header_sync_error; 
      test_case "test error: bad length" `Slow test_header_bad_length_error;
      test_case "test error: bad message type" `Slow test_header_bad_message_type;
      test_case "test error: duplicated attr" `Slow test_update_duplicated_attr;
      test_case "test error: missing well known attr" `Slow test_update_missing_well_known_attr;
      test_case "test error: attribute flags error" `Slow test_update_attr_flags_err;
      test_case "test error: attribute length error" `Slow test_update_attr_length_err;
      test_case "test error: unrecognised wellknwon attr" `Slow test_update_attr_unrecognized_wn_attr;
      test_case "test error: invalid origin attribute" `Slow test_update_invalid_origin;
    ];
  ]
;;






