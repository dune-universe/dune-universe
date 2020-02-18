open OUnit

let redis_test_host () =
  try
    Sys.getenv("OCAML_REDIS_TEST_IP")
  with Not_found ->
    "127.0.0.1"

let redis_test_port () =
  try
    int_of_string(Sys.getenv("OCAML_REDIS_TEST_PORT"))
  with Not_found ->
    6379

let redis_string_bucket () =
  let number = Random.bits () in
  "ounit_" ^ string_of_int(number)

let redis_integer_bucket = Random.bits

let redis_float_bucket () =
  (* Redis' float operations precision differs from OCaml's float operation
     precision.  Limit our floats to 11 digits after the decimal point to have
     possibility to test float operations. *)
  let a = float_of_int (Random.bits ()) in
  let b = float_of_int (Random.bits ()) in
  float_of_string (Printf.sprintf "%.8f" (a /. b))

let redis_n_strings_bucket n =
  let rec helper acc n =
    if n = 0 then acc else helper (redis_string_bucket () :: acc) (n - 1) in
  helper [] n

let rec test_exit_code = function
  | [] ->
    0
  | RSuccess _ :: t
  | RSkip _ :: t ->
    test_exit_code t
  | RFailure _ :: _
  | RError   _ :: _
  | RTodo    _ :: _ ->
    1

module Make(Client : Redis.S.Client) : sig
  val suite : string -> OUnit.test
  val teardown : unit -> unit
  val redis_spec : Client.connection_spec
  val bracket : (Client.connection -> 'a Client.IO.t) -> unit -> 'a
end = struct

  module IO = Client.IO

  let (>>=) = IO.(>>=)
  let (>>|) x f = x >>= fun x -> IO.return (f x)

  let redis_spec : Client.connection_spec =
    Client.({host=redis_test_host ();
             port=redis_test_port () })

  let io_assert msg check result =
    IO.return (assert_bool msg (check result))

  (* PING *)
  let test_case_ping conn =
    Client.ping conn >>=
    io_assert "Can't connect to Redis server" ((=) true)

  (* ECHO *)
  let test_case_echo conn =
    Client.echo conn "ECHO" >>=
    io_assert "Can't echo to Redis server" ((=) (Some "ECHO"))

  (* INFO *)
  let test_case_info conn =
    Client.info conn >>| fun result ->
    (let tcp_port =
       CCList.find_map (fun (k,v) -> if k = "tcp_port" then Some v else None) result
       |> CCOpt.get_lazy (fun () -> assert_failure "didn't find any port")
     in
     assert_bool "Got wrong data about port with INFO command"
       (int_of_string tcp_port = redis_test_port()))

  (* Keys test case *)
  let test_case_keys conn =
    let key = redis_string_bucket () in
    let value = redis_string_bucket () in
    let key' = redis_string_bucket () in
    let key'' = redis_string_bucket () in

    Client.object_encoding conn key >>=
    io_assert "Unexpected encoding for empty key" ((=) None) >>= fun () ->
    Client.object_idletime conn key >>=
    io_assert "Unexpected idletime for empty key" ((=) None) >>= fun () ->
    Client.object_refcount conn key >>=

    io_assert "Unexpected refcount for empty key" ((=) None) >>= fun () ->
    Client.set conn key value >>=
    io_assert "Can't set key" ((=) true) >>= fun () ->
    Client.set conn ~xx:true key value >>=
    io_assert "Can set xx key which is already set" ((=) true) >>= fun () ->
    Client.setnx conn key value >>=
    io_assert "Can setnx key which is already set" ((=) false) >>= fun () ->
    Client.set conn ~nx:true key value >>=
    io_assert "Can set nx key which is already set" ((=) false) >>= fun () ->
    Client.set conn ~ex:20 key value >>=
    io_assert "Can set ex key which is already set" ((=) true) >>= fun () ->
    Client.set conn ~px:200 key value >>=
    io_assert "Can set ex key which is already set" ((=) true) >>= fun () ->
    Client.get conn key >>=
    io_assert "Key and value mismatch" ((=) (Some value)) >>= fun () ->
    Client.getset conn key value >>=
    io_assert "Got unexpected value" ((=) (Some value)) >>= fun () ->
    Client.getrange conn key 0 (String.length value) >>=
    io_assert "Value and it's getrange copy differs" ((=) (Some value)) >>= fun () ->
    Client.setrange conn key 0 value >>=
    io_assert "Value and it's copy setrange result differs" ((=) (String.length value)) >>= fun () ->
    Client.strlen conn key >>=
    io_assert "Value length and it's strlen differs" ((=) (String.length value)) >>= fun () ->

    Client.object_encoding conn key >>=
    io_assert "Unexpected encoding for raw value" ((=) (Some "raw")) >>= fun () ->
    Client.object_idletime conn key >>=
    io_assert "Unexpected idletime for just requested key" ((=) (Some 0)) >>= fun () ->
    Client.object_refcount conn key >>=
    io_assert "Unexpected refcount for referenced key" ((=) (Some 1)) >>= fun () ->

    Client.exists conn key >>=
    io_assert "Key doesn't exist" ((=) true) >>= fun () ->
    Client.keys conn key >>=
    io_assert "Can't find with itself as a pattern in KEYS command"
      (fun keys -> let found_key = List.find (fun k -> k = key) keys in
        (found_key = key)) >>= fun () ->
    Client.randomkey conn >>=
    io_assert "Can't find key with RANDOMKEY command" ((<>) None) >>= fun () ->
    Client.move conn key 2 >>=
    io_assert "Can't move key to redis database #2" ((=) true) >>= fun () ->
    Client.select conn 2 >>=
    io_assert "Can't select redis database #2" ((=) ()) >>= fun () ->
    Client.rename conn key key' >>=
    io_assert "Can't rename key" ((=) ()) >>= fun () ->
    Client.set conn key'' value >>=
    io_assert "Can't set key''" ((=) true) >>= fun () ->
    Client.renamenx conn key' key'' >>=
    io_assert "Can renamenx key" ((=) false) >>= fun () ->
    Client.rename conn key' key >>=
    io_assert "Can't rename key" ((=) ()) >>= fun () ->
    Client.del conn [key; key''] >>=
    io_assert "Key wasn't deleted" ((=) 2) >>= fun () ->
    Client.select conn 0 >>=
    io_assert "Can't select redis database #0" ((=) ())

  let test_case_multiple_keys conn =
    let keys = redis_n_strings_bucket 10 in
    let values = List.rev keys in
    let kv_pairs = List.combine keys values in
    Client.mset conn kv_pairs >>=
    io_assert "Can't set multiple keys" ((=) ()) >>= fun () ->

    let expected_values = List.map (fun x -> Some x) values in
    Client.mget conn keys >>| (fun actual_values ->
        (List.iter2
           (fun expected actual -> assert_bool "Got unexpected value" (expected = actual))
           expected_values actual_values)) >>= fun () ->

    let another_values = redis_n_strings_bucket 10 in
    let kv_pairs = List.combine keys another_values in
    Client.msetnx conn kv_pairs >>=
    io_assert "It's possible MSETNX multiple keys" ((=) false) >>= fun () ->

    let another_keys = redis_n_strings_bucket 10 in
    let kv_pairs = List.combine another_keys another_values in
    Client.msetnx conn kv_pairs >>=
    io_assert "Can't MSETNX multiple keys" ((=) true)

  let test_case_dump_restore conn =
    let key = redis_string_bucket () in
    let value = redis_string_bucket () in
    Client.set conn key value >>=
    io_assert "Can't set key" ((=) true) >>= fun () ->
    Client.dump conn key >>= function
    | None -> assert_failure "Can't dump value"
    | Some value_dump ->
      let key' = String.concat "" [key; redis_string_bucket ()] in
      Client.restore conn key' 0 value_dump >>=
      io_assert "Can't restore value" ((=) ()) >>= fun () ->
      Client.get conn key' >>=
      io_assert "Key value and restored value mismatch" ((=) (Some value))

  let test_case_expire conn =
    let key = redis_string_bucket () in
    let value = redis_string_bucket () in
    Client.set conn key value >>=
    io_assert "Can't set key" ((=) true) >>= fun () ->
    Client.setex conn key 1 value >>=
    io_assert "Can't setex key" ((=) ()) >>= fun () ->
    Client.psetex conn key 1000 value >>=
    io_assert "Can't psetex key" ((=) ()) >>= fun () ->
    Client.ttl conn key >>=
    io_assert "Can't check expiration timeout for key"
      (fun x -> List.mem x [Some 0; Some 1]) >>= fun () ->
    Client.pttl conn key >>| (function
        | Some pttl -> assert_bool "Expiration timeout differs from setted" (0 <= pttl && pttl <= 1000)
        | None -> assert_failure "Can't check expiration timeout for key")
    >>= fun () ->
    Client.expire conn key 1 >>=
    io_assert "Can't set expiration timeout for key" ((=) true) >>= fun () ->
    Client.pexpire conn key 1000 >>=
    io_assert "Can't set expiration timeout in milliseconds for key" ((=) true) >>= fun () ->
    Client.ttl conn key >>=
    io_assert "Can't check expiration timeout for key"
      (fun x -> List.mem x [Some 0; Some 1]) >>= fun () ->
    Client.pttl conn key >>| (function
        | Some pttl -> assert_bool "Expiration timeout differs from setted" (0 <= pttl && pttl <= 1000)
        | None -> assert_failure "Can't check expiration timeout for key")
    >>= fun () ->
    Client.persist conn key >>=
    io_assert "Can't remove existing timeout on key" ((=) true)  >>= fun () ->
    Client.ttl conn key >>=
    io_assert "Can't check expiration timeout for key" ((=) None)

  let test_case_expireat conn =
    let key = redis_string_bucket () in
    let value = redis_string_bucket () in
    Client.set conn key value >>=
    io_assert "Can't set key" ((=) true) >>= fun () ->
    let expiry = Unix.time () +. 1. in
    Client.expireat conn key expiry >>=
    io_assert "Can't set expiration timeout for key" ((=) true) >>= fun () ->
    Client.ttl conn key >>=
    io_assert "Can't check expiration timeout for key"
      (fun x -> List.mem x [Some 0; Some 1]) >>= fun () ->

    let pexpiry = int_of_float (Unix.time ()) * 1000 + 1000 in
    Client.pexpireat conn key pexpiry >>=
    io_assert "Can't set expiration timeout for key (in ms)" ((=) true) >>= fun () ->
    Client.pttl conn key >>| function
    | Some pttl ->
      assert_bool "Expiration timeout differs from setted" (0 <= pttl && pttl <= 1000)
    | None ->
      assert_failure "Can't check expiration timeout for key"

  let test_case_type conn =
    let value = redis_string_bucket () in
    let string_key = redis_string_bucket () in
    Client.set conn string_key value >>=
    io_assert "Can't set key" ((=) true) >>= fun () ->
    let list_key = redis_string_bucket () in
    Client.lpush conn list_key [value] >>=
    io_assert "Can't push value to list" ((=) 1) >>= fun () ->

    Client.type_of conn string_key >>=
    io_assert "Got wrong key type for string_key" ((=) `String) >>= fun () ->
    Client.type_of conn list_key >>=
    io_assert "Got wrong key type for list_key" ((=) `List)

  (* APPEND *)
  let test_case_append conn =
    let key = redis_string_bucket () in
    let value = redis_string_bucket () in
    Client.append conn key value >>=
    io_assert "Can't append initial value to key"
      (fun x -> x = String.length value) >>= fun () ->
    Client.append conn key value >>=
    io_assert "Can't append additional value to key"
      (fun x -> x = (String.length value + String.length value)) >>= fun () ->
    Client.get conn key >>=
    io_assert "Can't get key" ((=) (Some (String.concat "" [value; value])))

  (* INCR/DECR/INCRBY/DECRBY/INCRBYFLOAT *)
  let test_case_incr_decr conn =
    let key = redis_string_bucket () in
    let value = redis_integer_bucket () in
    let increment = redis_integer_bucket () in
    Client.set conn key (string_of_int value) >>=
    io_assert "Can't set float value to key" ((=) true) >>= fun () ->
    Client.incrby conn key increment >>=
    io_assert "Can't increment value by integer" ((=) (value + increment)) >>= fun () ->
    Client.incr conn key >>=
    io_assert "Can't increment value by one" ((=) (value + increment + 1)) >>= fun () ->
    Client.decrby conn key increment >>=
    io_assert "Can't decrement value by integer" ((=) (value + 1)) >>= fun () ->
    Client.decr conn key >>=
    io_assert "Can't decrement value by one" ((=) value) >>= fun () ->
    Client.incrbyfloat conn key 2. >>=
    io_assert "Can't increment value by float"
      ((=) (float_of_int value +. 2.)) >>= fun () ->
    Client.incrbyfloat conn key (- 2.) >>=
    io_assert "Can't increment value by negative float"
      ((=) (float_of_int value))

  (* BITOP/BITCOUNT/BITPOS/GETBIT/SETBIT *)
  let test_case_bit_operations conn =
    let dest = redis_string_bucket () in
    let key1 = redis_string_bucket () in
    let key2 = redis_string_bucket () in
    let value1 = "foobar" in
    let value2 = "abcdef" in
    let value3 = "\x00\xff\xf0" in
    Client.set conn key1 value1 >>=
    io_assert "Can't set value1 to key1" ((=) true) >>= fun () ->
    Client.set conn key2 value2 >>=
    io_assert "Can't set value2 to key2" ((=) true) >>= fun () ->
    Client.bitop conn Client.AND dest [key1; key2] >>=
    io_assert "Can't execute BITOP AND key1 and key2" ((=) 6) >>= fun () ->
    Client.get conn dest >>=
    io_assert "Got unexpected value from dest" ((=) (Some "`bc`ab")) >>= fun () ->
    Client.bitop conn Client.NOT dest [key1] >>=
    io_assert "Can't execute BITOP NOT key1" ((=) 6) >>= fun () ->
    Client.get conn dest >>=
    io_assert "Got unexpected value from dest" ((=) (Some "\x99\x90\x90\x9d\x9e\x8d")) >>= fun () ->
    Client.set conn key1 value3 >>=
    io_assert "Can't set value3 to key1" ((=) true) >>= fun () ->
    Client.bitpos conn key1 1 >>=
    io_assert "Got unexpected bit position" ((=) 8) >>= fun () ->
    Client.bitpos conn key1 1 ~first:0 >>=
    io_assert "Got unexpected bit position" ((=) 8) >>= fun () ->
    Client.bitpos conn key1 1 ~first:2 >>=
    io_assert "Got unexpected bit position" ((=) 16) >>= fun () ->
    Client.getbit conn key1 0 >>=
    io_assert "Can't get bit" ((=) 0) >>= fun () ->
    Client.setbit conn key1 0 1 >>=
    io_assert "Can't set bit" ((=) 0) >>= fun () ->
    Client.getbit conn key1 0 >>=
    io_assert "Can't get bit" ((=) 1) >>= fun () ->
    Client.set conn key1 value1 >>=
    io_assert "Can't set value1 to key1" ((=) true) >>= fun () ->
    Client.bitcount conn key1 >>=
    io_assert "Got unexpected bit count" ((=) 26) >>= fun () ->
    Client.bitcount conn key1 ~first:1 >>=
    io_assert "Got unexpected bit count" ((=) 22) >>= fun () ->
    Client.bitcount conn key1 ~first:0 ~last:0 >>=
    io_assert "Got unexpected bit count" ((=) 4) >>= fun () ->
    Client.bitcount conn key1 ~first:1 ~last:1 >>=
    io_assert "Got unexpected bit count" ((=) 6)

  let test_case_scan conn =
    let rec scan_keys cursor keys =
      Client.scan conn cursor >>= fun (next_cursor, next_keys) ->
      let next_keys = List.concat [keys; next_keys] in
      if next_cursor = 0 then
        IO.return next_keys
      else
        scan_keys next_cursor next_keys
    in
    let scan_all_keys () = scan_keys 0 [] in
    Client.keys conn "*" >>= fun keys ->
    scan_all_keys () >>=
    io_assert "Number of keys got with KEYS command is not equal to number of keys got with SCAN command"
      (fun scanned_keys -> List.length keys = List.length scanned_keys)

  let test_case_list conn =
    let key = redis_string_bucket () in
    let value1 = redis_string_bucket () in
    let value2 = redis_string_bucket () in
    Client.lpush conn key [value1] >>=
    io_assert "Got unexpected list length" ((=) 1) >>= fun () ->
    Client.rpush conn key [value2] >>=
    io_assert "Got unexpected list length" ((=) 2) >>= fun () ->
    Client.lrange conn key 0 2 >>=
    io_assert "Got unexpected list contents" ((=) [value1; value2]) >>= fun () ->
    Client.del conn [key] >>= fun _ ->
    let key = redis_string_bucket () in
    Client.lpush conn key [value1] >>=
    io_assert "Got unexpected list length" ((=) 1) >>= fun () ->
    Client.blpop conn [key] 1 >>=
    io_assert "Got unexpected value" ((=) (Some (key, value1))) >>= fun () ->
    Client.del conn [key] >>= fun _ -> IO.return ()

  let test_case_hash conn =
    let key = redis_string_bucket () in
    let field = redis_string_bucket () in
    let value = redis_string_bucket () in
    Client.hmset conn key [(field, value)] >>=
    io_assert "Can not set multiple fields for hash" ((=) ()) >>= fun () ->
    Client.hget conn key field >>=
    io_assert "Got unexpected value of the field" ((=) (Some value)) >>= fun () ->
    Client.hlen conn key >>=
    io_assert "Got unexpected hash size" ((=) 1) >>= fun () ->
    Client.hdel conn key field >>=
    io_assert "Got unexpected result of field deletion" ((=) true) >>= fun () ->
    let new_value = redis_integer_bucket () in
    let new_value_s = string_of_int new_value in
    Client.hset conn key field new_value_s >>=
    io_assert "Can not set value for hash field" ((=) true) >>= fun _ ->
    Client.hset conn key field new_value_s >>=
    io_assert "Result of hash set is unexpected" ((=) false) >>= fun _ ->
    Client.hincrbyfloat conn key field 1.0 >>=
    io_assert "Got unexpected value" ((=) ((float_of_int new_value) +. 1.0))

  let test_case_hyper_log_log conn =
    let key1 = redis_string_bucket () in
    let key2 = redis_string_bucket () in
    Client.pfadd conn key1 ["a"; "b"; "c"] >>=
    io_assert "Can not add items to hyperloglog" ((=) true) >>= fun () ->
    Client.pfadd conn key1 ["a"; "b"; "c"] >>=
    io_assert "Can not add items to hyperloglog" ((=) false) >>= fun () ->
    Client.pfcount conn [key1] >>=
    io_assert "Got wrong items count" ((=) 3) >>= fun () ->
    Client.pfadd conn key2 ["d"; "e"; "f"] >>=
    io_assert "Can not add items to hyperloglog" ((=) true) >>= fun () ->
    Client.pfcount conn [key1; key2] >>=
    io_assert "Got wrong items count" ((=) 6) >>= fun () ->
    Client.pfmerge conn [key1; key2] >>=
    io_assert "Got wrong items count" ((=) ())

  let test_case_hscan conn =
    let key = redis_string_bucket () in
    let fields = redis_n_strings_bucket 10 in
    let pairs = List.map (fun f -> (f, f)) fields in

    Client.hmset conn key pairs >>=
    io_assert "Can not set multiple fields for hash" ((=) ()) >>= fun () ->

    let rec hscan_fields key cursor fields =
      Client.hscan conn key cursor >>= fun (next_cursor, next_fields) ->
      let next_fields = List.concat [fields; next_fields] in
      if next_cursor == 0 then
        IO.return next_fields
      else
        hscan_fields key next_cursor next_fields in
    let hscan_all_fields () = hscan_fields key 0 [] in

    Client.hkeys conn key >>= fun fields ->
    hscan_all_fields () >>=
    io_assert "Number of keys got with HKEYS command is not equal to number of keys got with HSCAN command"
      (fun scanned_fields ->
         List.length fields = List.length scanned_fields)

  let test_case_sorted_set conn =
    let key = redis_string_bucket () in
    Client.zadd conn key [ 1.4, "obj1"; 1.6, "obj2"; ] >>=
    io_assert "added 2 items to set" ((=) 2) >>= fun () ->
    Client.zadd conn key ~x:`NX [ 1.4, "obj1"; 1.6, "obj2"; ] >>=
    io_assert "no elements were added" ((=) 0) >>= fun () ->
    Client.zadd conn key ~x:`XX [ 1.4, "obj1"; 1.6, "obj2"; ] >>=
    io_assert "no elements were added" ((=) 0) >>= fun () ->
    Client.zadd conn key ~ch:true [ 3., "obj1"; 3., "obj2"; ] >>=
    io_assert "2 elements were changed" ((=) 2) >>= fun () ->
    Client.zincrby conn key 2. "obj1" >>= fun _ ->
    Client.zscore conn key "obj1" >>=
    io_assert "score of obj1 should be 5 now" ((=) (Some 5.)) >>= fun () ->
    Client.zrange conn key 0 100 >>= fun _ ->
    Client.zrevrange conn key 0 100 >>= fun _ ->
    let open Client.FloatBound in
    Client.zrangebyscore conn key NegInfinity PosInfinity >>= fun _ ->
    let open Client.FloatBound in
    Client.zrevrangebyscore conn key NegInfinity PosInfinity >>= fun _ ->
    let open Client.StringBound in
    Client.zrangebylex conn key NegInfinity PosInfinity >>= fun _ ->
    let open Client.StringBound in
    Client.zrevrangebylex conn key NegInfinity PosInfinity >>= fun _ ->
    let open Client.FloatBound in
    Client.zcount conn key NegInfinity PosInfinity >>=
    io_assert "wrong sorted set size returned" ((=) 2) >>= fun () ->
    let open Client.FloatBound in
    Client.zcount conn key (Inclusive 5.) (Exclusive 100.)  >>=
    io_assert "wrong sorted set size returned" ((=) 1) >>= fun () ->
    let open Client.StringBound in
    Client.zlexcount conn key NegInfinity PosInfinity >>=
    io_assert "wrong sorted set size returned" ((=) 2) >>= fun () ->
    let open Client.StringBound in
    Client.zlexcount conn key (Inclusive "obj") (PosInfinity) >>=
    io_assert "wrong sorted set size returned" ((=) 2) >>= fun () ->
    Client.zrem conn key [ "obj1"; "non_existing_key"; ] >>=
    io_assert "not removed 1 item from set" ((=) 1) >>= fun () ->
    Client.zrank conn key "obj1" >>=
    io_assert "returned wrong rank" ((=) (None)) >>= fun () ->
    Client.zrank conn key "obj2" >>=
    io_assert "returned wrong rank" ((=) (Some 0)) >>= fun () ->
    Client.zrevrank conn key "obj1" >>=
    io_assert "returned wrong rank" ((=) (None)) >>= fun () ->
    Client.zrevrank conn key "obj2" >>=
    io_assert "returned wrong rank" ((=) (Some 0)) >>= fun () ->
    Client.zcard conn key >>=
    io_assert "wrong sorted set size returned" ((=) 1) >>= fun () ->
    Client.zscore conn key "obj1" >>=
    io_assert "item was removed" ((=) None)

  let test_case_sorted_set_remove conn =
    let key = redis_string_bucket () in
    Client.zadd conn key [ 1.4, "obj1"; 1.6, "obj2"; ] >>= fun _ ->
    Client.zremrangebyrank conn key 0 100 >>=
    io_assert "removed wrong number of items" ((=) 2) >>= fun _ ->
    Client.zadd conn key [ 1.4, "obj1"; 1.6, "obj2"; ] >>= fun _ ->
    let open Client.FloatBound in
    Client.zremrangebyscore conn key (Inclusive 1.) (Exclusive 2.) >>=
    io_assert "removed wrong number of items" ((=) 2) >>= fun _ ->
    Client.zadd conn key [ 1.4, "obj1"; 1.6, "obj2"; ] >>= fun _ ->
    let open Client.StringBound in
    Client.zremrangebylex conn key NegInfinity PosInfinity >>=
    io_assert "removed wrong number of items" ((=) 2)

  let cleanup_keys conn =
    Client.keys conn "ounit_*" >>= fun keys ->
    Client.del conn keys >>= fun _ ->
    IO.return ()

  let bracket test_case () =
    try
      IO.run @@ Client.with_connection redis_spec test_case
    with (Client.Unexpected reply as exn) ->
      let rec to_string = function
        | `Status s -> Printf.sprintf "(Status %s)" s
        | `Moved {Client.slot; host; port} -> Printf.sprintf "MOVED %d %s:%i" slot host port
        | `Ask {Client.slot; host; port} -> Printf.sprintf "ASK %d %s:%i" slot host port
        | `Error  s -> Printf.sprintf "(Error %s)" s
        | `Int i -> Printf.sprintf "(Int %i)" i
        | `Int64 i -> Printf.sprintf "(Int64 %Li)" i
        | `Bulk None -> "(Bulk None)"
        | `Bulk (Some s) -> Printf.sprintf "(Bulk (Some %s))" s
        | `Multibulk replies ->
          let x = List.map to_string replies |> String.concat "; " in
          Printf.sprintf "Multibulk [ %s; ]" x
      in
      Printf.eprintf "Got unexpected reply: %s\n" (to_string reply);
      raise exn

  let teardown () =
    flush stderr;
    IO.run @@ Client.with_connection redis_spec cleanup_keys

  let suite name =
    let suite_name = "redis." ^ name in
    suite_name >::: [
        "test_case_ping" >:: (bracket test_case_ping);
        "test_case_echo" >:: (bracket test_case_echo);
        "test_case_info" >:: (bracket test_case_info);
        "test_case_keys" >:: (bracket test_case_keys);
        "test_case_multiple_keys" >:: (bracket test_case_multiple_keys);
        "test_case_dump_restore" >:: (bracket test_case_dump_restore);
        "test_case_expire" >:: (bracket test_case_expire);
        "test_case_expireat" >:: (bracket test_case_expireat);
        "test_case_type" >:: (bracket test_case_type);
        "test_case_append" >:: (bracket test_case_append);
        "test_case_incr_decr" >:: (bracket test_case_incr_decr);
        "test_case_bit_operations" >:: (bracket test_case_bit_operations);
        "test_case_scan" >:: (bracket test_case_scan);
        "test_case_list" >:: (bracket test_case_list);
        "test_case_hash" >:: (bracket test_case_hash);
        "test_case_hscan" >:: (bracket test_case_hscan);
        "test_case_hyper_log_log" >:: (bracket test_case_hyper_log_log);
        "test_case_sorted_set" >:: (bracket test_case_sorted_set);
        "test_case_sorted_set_remove" >:: (bracket test_case_sorted_set_remove);
      ]
end
