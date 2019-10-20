open Core
open Async

type common_error =
  [ `Connection_closed
  | `Unexpected ]
[@@deriving show, eq]

type response = (Resp.t, common_error) result

type command = string list

type request = {
  command : command;
  waiter : response Ivar.t
}

let construct_request commands =
  commands
  |> List.map ~f:(fun cmd -> Resp.Bulk cmd)
  |> (fun xs -> Resp.Array xs)
  |> Resp.encode

type t = {
  (* Need a queue of waiter Ivars. Need some way of closing the connection *)
  waiters : response Ivar.t Queue.t;
  reader : request Pipe.Reader.t;
  writer : request Pipe.Writer.t
}

let init reader writer =
  let waiters = Queue.create () in
  let rec recv_loop reader =
    match%bind Monitor.try_with_or_error @@ fun () -> Parser.read_resp reader with
    | Error _ | Ok (Error _) -> return ()
    | Ok (Ok r as result) -> (
      match Queue.dequeue waiters with
      | None when Reader.is_closed reader -> return ()
      | None -> failwithf "No waiters are waiting for this message: %s" (Resp.show r) ()
      | Some waiter -> Ivar.fill waiter result; recv_loop reader )
  in
  (* Requests are posted to a pipe, and requests are processed in sequence *)
  let request_reader, request_writer = Pipe.create () in
  let handle_request {command; waiter} =
    Queue.enqueue waiters waiter;
    let request = construct_request command in
    return @@ Writer.write writer request
  in
  (* Start redis receiver. Processing ends if the connection is closed. *)
  don't_wait_for
    (let%bind () = recv_loop reader in
     return @@ Pipe.close request_writer);
  (* Start processing requests. Once the pipe is closed, we signal
     closed to all outstanding waiters after closing the underlying
     socket *)
  don't_wait_for
    (let%bind () = Pipe.iter request_reader ~f:handle_request in
     let%bind () = Writer.close writer in
     let%bind () = Reader.close reader in
     (* Signal this to all waiters. As the pipe has been closed, we
        know that no new waiters will arrive *)
     Queue.iter waiters ~f:(fun waiter -> Ivar.fill waiter @@ Error `Connection_closed);
     return @@ Queue.clear waiters);
  {waiters; reader = request_reader; writer = request_writer}

let connect ?(port = 6379) ~host =
  let where =
    Tcp.Where_to_connect.of_host_and_port @@ Host_and_port.create ~host ~port
  in
  let%bind _socket, reader, writer = Tcp.connect where in
  return @@ init reader writer

let close {writer; _} = return @@ Pipe.close writer

let request t command =
  match Pipe.is_closed t.writer with
  | true -> return @@ Error `Connection_closed
  | false -> (
      let waiter = Ivar.create () in
      let%bind () = Pipe.write t.writer {command; waiter} in
      (* Type coercion: [common_error] -> [> common_error] *)
      match%map Ivar.read waiter with
      | Ok _ as res -> res
      | Error `Connection_closed -> Error `Connection_closed
      | Error `Unexpected -> Error `Unexpected )

let echo t message =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["ECHO"; message] with
  | Resp.Bulk v -> return v
  | _ -> Deferred.return @@ Error `Unexpected

type exist =
  | Always
  | Not_if_exists
  | Only_if_exists

let set t ~key ?expire ?(exist = Always) value =
  let open Deferred.Result.Let_syntax in
  let expiry =
    match expire with
    | None -> []
    | Some span -> ["PX"; span |> Time.Span.to_ms |> int_of_float |> string_of_int]
  in
  let existence =
    match exist with
    | Always -> []
    | Not_if_exists -> ["NX"]
    | Only_if_exists -> ["XX"]
  in
  let command = ["SET"; key; value] @ expiry @ existence in
  match%bind request t command with
  | Resp.Null -> return false
  | Resp.String "OK" -> return true
  | _ -> Deferred.return @@ Error `Unexpected

let get t key =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["GET"; key] with
  | Resp.Bulk v -> return @@ Some v
  | Resp.Null -> return @@ None
  | _ -> Deferred.return @@ Error `Unexpected

let getrange t ~start ~end' key =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["GETRANGE"; key; string_of_int start; string_of_int end'] with
  | Resp.Bulk v -> return v
  | _ -> Deferred.return @@ Error `Unexpected

let getset t ~key value =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["GETSET"; key; value] with
  | Resp.Bulk v -> return (Some v)
  | Resp.Null -> return None
  | _ -> Deferred.return @@ Error `Unexpected

let strlen t key =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["STRLEN"; key] with
  | Resp.Integer v -> return v
  | _ -> Deferred.return @@ Error `Unexpected

let mget t keys =
  let open Deferred.Result.Let_syntax in
  match%bind request t ("MGET" :: keys) with
  | Resp.Array xs ->
      xs
      |> List.fold_right ~init:(Ok []) ~f:(fun item acc ->
             match acc with
             | Error _ -> acc
             | Ok acc -> (
               match item with
               | Resp.Null -> Ok (None :: acc)
               | Resp.Bulk s -> Ok (Some s :: acc)
               | _ -> Error `Unexpected ) )
      |> Deferred.return
  | _ -> Deferred.return @@ Error `Unexpected

let mset t alist =
  let open Deferred.Result.Let_syntax in
  let payload = alist |> List.map ~f:(fun (k, v) -> [k; v]) |> List.concat in
  match%bind request t ("MSET" :: payload) with
  | Resp.String "OK" -> return ()
  | _ -> Deferred.return @@ Error `Unexpected

let msetnx t alist =
  let open Deferred.Result.Let_syntax in
  let payload = alist |> List.map ~f:(fun (k, v) -> [k; v]) |> List.concat in
  match%bind request t ("MSETNX" :: payload) with
  | Resp.Integer 1 -> return true
  | Resp.Integer 0 -> return false
  | _ -> Deferred.return @@ Error `Unexpected

let lpush t ~key value =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["LPUSH"; key; value] with
  | Resp.Integer n -> return n
  | _ -> Deferred.return @@ Error `Unexpected

let lrange t ~key ~start ~stop =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["LRANGE"; key; string_of_int start; string_of_int stop] with
  | Resp.Array xs ->
      List.map xs ~f:(function
          | Resp.Bulk v -> Ok v
          | _ -> Error `Unexpected )
      |> Result.all
      |> Deferred.return
  | _ -> Deferred.return @@ Error `Unexpected

let append t ~key value =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["APPEND"; key; value] with
  | Resp.Integer n -> return n
  | _ -> Deferred.return @@ Error `Unexpected

let auth t password =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["AUTH"; password] with
  | Resp.String "OK" -> return ()
  | Resp.Error e -> Deferred.return @@ Error (`Redis_error e)
  | _ -> Deferred.return @@ Error `Unexpected

let bgrewriteaof t =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["BGREWRITEAOF"] with
  (* the documentation says it returns OK, but that's not true *)
  | Resp.String v -> return v
  | _ -> Deferred.return @@ Error `Unexpected

let bgsave t =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["BGSAVE"] with
  | Resp.String v -> return v
  | _ -> Deferred.return @@ Error `Unexpected

let bitcount t ?range key =
  let open Deferred.Result.Let_syntax in
  let range =
    match range with
    | None -> []
    | Some (start, end_) -> [string_of_int start; string_of_int end_]
  in
  match%bind request t (["BITCOUNT"; key] @ range) with
  | Resp.Integer n -> return n
  | _ -> Deferred.return @@ Error `Unexpected

type overflow =
  | Wrap
  | Sat
  | Fail

let string_of_overflow = function
  | Wrap -> "WRAP"
  | Sat -> "SAT"
  | Fail -> "FAIL"

(* Declaration of type of the integer *)
type intsize =
  | Signed of int
  | Unsigned of int

let string_of_intsize = function
  | Signed v -> Printf.sprintf "i%d" v
  | Unsigned v -> Printf.sprintf "u%d" v

type offset =
  | Absolute of int
  | Relative of int

let string_of_offset = function
  | Absolute v -> string_of_int v
  | Relative v -> Printf.sprintf "#%d" v

type fieldop =
  | Get of intsize * offset
  | Set of intsize * offset * int
  | Incrby of intsize * offset * int

let bitfield t ?overflow key ops =
  let open Deferred.Result.Let_syntax in
  let ops =
    ops
    |> List.map ~f:(function
           | Get (size, offset) ->
               ["GET"; string_of_intsize size; string_of_offset offset]
           | Set (size, offset, value) ->
               [ "SET";
                 string_of_intsize size;
                 string_of_offset offset;
                 string_of_int value ]
           | Incrby (size, offset, increment) ->
               [ "INCRBY";
                 string_of_intsize size;
                 string_of_offset offset;
                 string_of_int increment ] )
    |> List.concat
  in
  let overflow =
    match overflow with
    | None -> []
    | Some behaviour -> ["OVERFLOW"; string_of_overflow behaviour]
  in
  match%bind request t (["BITFIELD"; key] @ overflow @ ops) with
  | Resp.Array xs ->
      let open Result.Let_syntax in
      xs
      |> List.fold ~init:(Ok []) ~f:(fun acc v ->
             match acc, v with
             | Error _, _ -> acc
             | Ok acc, Resp.Integer i -> Ok (Some i :: acc)
             | Ok acc, Resp.Null -> Ok (None :: acc)
             | Ok _, _ -> Error `Unexpected )
      >>| List.rev
      |> Deferred.return
  | _ -> Deferred.return @@ Error `Unexpected

type bitop =
  | AND
  | OR
  | XOR
  | NOT

let string_of_bitop = function
  | AND -> "AND"
  | OR -> "OR"
  | XOR -> "XOR"
  | NOT -> "NOT"

let bitop t ~destkey ?(keys = []) ~key op =
  let open Deferred.Result.Let_syntax in
  match%bind request t (["BITOP"; string_of_bitop op; destkey; key] @ keys) with
  | Resp.Integer n -> return n
  | _ -> Deferred.return @@ Error `Unexpected

type bit =
  | Zero
  | One
[@@deriving show, eq]

let string_of_bit = function
  | Zero -> "0"
  | One -> "1"

let bitpos t ?start ?end' key bit =
  let open Deferred.Result.Let_syntax in
  let%bind range =
    match start, end' with
    | Some s, Some e -> return [string_of_int s; string_of_int e]
    | Some s, None -> return [string_of_int s]
    | None, None -> return []
    | None, Some _ -> raise (Invalid_argument "Can't specify end without start")
  in
  match%bind request t (["BITPOS"; key; string_of_bit bit] @ range) with
  | Resp.Integer -1 -> return None
  | Resp.Integer n -> return @@ Some n
  | _ -> Deferred.return @@ Error `Unexpected

let getbit t key offset =
  let open Deferred.Result.Let_syntax in
  let offset = string_of_int offset in
  match%bind request t ["GETBIT"; key; offset] with
  | Resp.Integer 0 -> return Zero
  | Resp.Integer 1 -> return One
  | _ -> Deferred.return @@ Error `Unexpected

let setbit t key offset value =
  let open Deferred.Result.Let_syntax in
  let offset = string_of_int offset in
  let value = string_of_bit value in
  match%bind request t ["SETBIT"; key; offset; value] with
  | Resp.Integer 0 -> return Zero
  | Resp.Integer 1 -> return One
  | _ -> Deferred.return @@ Error `Unexpected

let decr t key =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["DECR"; key] with
  | Resp.Integer n -> return n
  | _ -> Deferred.return @@ Error `Unexpected

let decrby t key decrement =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["DECRBY"; key; string_of_int decrement] with
  | Resp.Integer n -> return n
  | _ -> Deferred.return @@ Error `Unexpected

let incr t key =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["INCR"; key] with
  | Resp.Integer n -> return n
  | _ -> Deferred.return @@ Error `Unexpected

let incrby t key increment =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["INCRBY"; key; string_of_int increment] with
  | Resp.Integer n -> return n
  | _ -> Deferred.return @@ Error `Unexpected

let incrbyfloat t key increment =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["INCRBYFLOAT"; key; string_of_float increment] with
  | Resp.Bulk v -> return @@ float_of_string v
  | _ -> Deferred.return @@ Error `Unexpected

let select t index =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["SELECT"; string_of_int index] with
  | Resp.String "OK" -> return ()
  | _ -> Deferred.return @@ Error `Unexpected

let del t ?(keys = []) key =
  let open Deferred.Result.Let_syntax in
  match%bind request t (["DEL"; key] @ keys) with
  | Resp.Integer n -> return n
  | _ -> Deferred.return @@ Error `Unexpected

let exists t ?(keys = []) key =
  let open Deferred.Result.Let_syntax in
  match%bind request t (["EXISTS"; key] @ keys) with
  | Resp.Integer n -> return n
  | _ -> Deferred.return @@ Error `Unexpected

let expire t key span =
  let open Deferred.Result.Let_syntax in
  let milliseconds = Time.Span.to_ms span in
  (* rounded to nearest millisecond *)
  let expire = Printf.sprintf "%.0f" milliseconds in
  match%bind request t ["PEXPIRE"; key; expire] with
  | Resp.Integer n -> return n
  | _ -> Deferred.return @@ Error `Unexpected

let expireat t key dt =
  let open Deferred.Result.Let_syntax in
  let since_epoch = dt |> Time.to_span_since_epoch |> Time.Span.to_ms in
  let expire = Printf.sprintf "%.0f" since_epoch in
  match%bind request t ["PEXPIREAT"; key; expire] with
  | Resp.Integer n -> return n
  | _ -> Deferred.return @@ Error `Unexpected

let coerce_bulk_array xs =
  xs
  |> List.map ~f:(function
         | Resp.Bulk key -> Ok key
         | _ -> Error `Unexpected )
  |> Result.all

let keys t pattern =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["KEYS"; pattern] with
  | Resp.Array xs -> xs |> coerce_bulk_array |> Deferred.return
  | _ -> Deferred.return @@ Error `Unexpected

let sadd t ~key ?(members = []) member =
  let open Deferred.Result.Let_syntax in
  match%bind request t ("SADD" :: key :: member :: members) with
  | Resp.Integer n -> return n
  | _ -> Deferred.return @@ Error `Unexpected

let scard t key =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["SCARD"; key] with
  | Resp.Integer n -> return n
  | _ -> Deferred.return @@ Error `Unexpected

let generic_setop setop t ?(keys = []) key =
  let open Deferred.Result.Let_syntax in
  match%bind request t (setop :: key :: keys) with
  | Resp.Array res -> res |> coerce_bulk_array |> Deferred.return
  | _ -> Deferred.return @@ Error `Unexpected

let sdiff = generic_setop "SDIFF"

let generic_setop_store setop t ~destination ?(keys = []) ~key =
  let open Deferred.Result.Let_syntax in
  match%bind request t (setop :: destination :: key :: keys) with
  | Resp.Integer n -> return n
  | _ -> Deferred.return @@ Error `Unexpected

let sdiffstore = generic_setop_store "SDIFFSTORE"

let sinter = generic_setop "SINTER"

let sinterstore = generic_setop_store "SINTERSTORE"

let sismember t ~key member =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["SISMEMBER"; key; member] with
  | Resp.Integer 0 -> return false
  | Resp.Integer 1 -> return true
  | _ -> Deferred.return @@ Error `Unexpected

let smembers t key =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["SMEMBERS"; key] with
  | Resp.Array res -> res |> coerce_bulk_array |> Deferred.return
  | _ -> Deferred.return @@ Error `Unexpected

let smove t ~source ~destination member =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["SISMEMBER"; source; destination; member] with
  | Resp.Integer 0 -> return false
  | Resp.Integer 1 -> return true
  | _ -> Deferred.return @@ Error `Unexpected

let spop t ?(count = 1) key =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["SPOP"; key; string_of_int count] with
  | Resp.Array res -> res |> coerce_bulk_array |> Deferred.return
  | _ -> Deferred.return @@ Error `Unexpected

let srandmember t ?(count = 1) key =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["SRANDMEMBER"; key; string_of_int count] with
  | Resp.Array res -> res |> coerce_bulk_array |> Deferred.return
  | _ -> Deferred.return @@ Error `Unexpected

let srem t ~key ?(members = []) member =
  let open Deferred.Result.Let_syntax in
  match%bind request t ("SREM" :: key :: member :: members) with
  | Resp.Integer n -> return n
  | _ -> Deferred.return @@ Error `Unexpected

let sunion = generic_setop "SUNION"

let sunionstore = generic_setop_store "SUNIONSTORE"

let generic_scan t ?pattern ?count over =
  let pattern =
    match pattern with
    | Some pattern -> ["MATCH"; pattern]
    | None -> []
  in
  let count =
    match count with
    | Some count -> ["COUNT"; string_of_int count]
    | None -> []
  in
  Pipe.create_reader ~close_on_exception:false @@ fun writer ->
  Deferred.repeat_until_finished "0" @@ fun cursor ->
  match%bind request t (over @ [cursor] @ pattern @ count) with
  | Ok (Resp.Array [Resp.Bulk cursor; Resp.Array from]) -> (
      let from =
        from
        |> List.map ~f:(function
               | Resp.Bulk s -> s
               | _ -> failwith "unexpected" )
        |> Queue.of_list
      in
      let%bind () = Pipe.transfer_in writer ~from in
      match cursor with
      | "0" -> return @@ `Finished ()
      | cursor -> return @@ `Repeat cursor )
  | _ -> failwith "unexpected"

let scan ?pattern ?count t = generic_scan t ?pattern ?count ["SCAN"]

let sscan t ?pattern ?count key = generic_scan t ?pattern ?count ["SSCAN"; key]

let move t key db =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["MOVE"; key; string_of_int db] with
  | Resp.Integer 0 -> return false
  | Resp.Integer 1 -> return true
  | _ -> Deferred.return @@ Error `Unexpected

let persist t key =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["PERSIST"; key] with
  | Resp.Integer 0 -> return false
  | Resp.Integer 1 -> return true
  | _ -> Deferred.return @@ Error `Unexpected

let randomkey t =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["RANDOMKEY"] with
  | Resp.Bulk s -> return s
  | _ -> Deferred.return @@ Error `Unexpected

let rename t key newkey =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["RENAME"; key; newkey] with
  | Resp.String "OK" -> return ()
  | _ -> Deferred.return @@ Error `Unexpected

let renamenx t ~key newkey =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["RENAMENX"; key; newkey] with
  | Resp.Integer 0 -> return false
  | Resp.Integer 1 -> return true
  | _ -> Deferred.return @@ Error `Unexpected

type order =
  | Asc
  | Desc

let sort t ?by ?limit ?get ?order ?alpha ?store key =
  let open Deferred.Result.Let_syntax in
  let by =
    match by with
    | None -> []
    | Some by -> ["BY"; by]
  in
  let limit =
    match limit with
    | None -> []
    | Some (offset, count) -> ["LIMIT"; string_of_int offset; string_of_int count]
  in
  let get =
    match get with
    | None -> []
    | Some patterns ->
        patterns |> List.map ~f:(fun pattern -> ["GET"; pattern]) |> List.concat
  in
  let order =
    match order with
    | None -> []
    | Some Asc -> ["ASC"]
    | Some Desc -> ["DESC"]
  in
  let alpha =
    match alpha with
    | None -> []
    | Some false -> []
    | Some true -> ["ALPHA"]
  in
  let store =
    match store with
    | None -> []
    | Some destination -> ["STORE"; destination]
  in
  let q = [["SORT"; key]; by; limit; get; order; alpha; store] |> List.concat in
  match%bind request t q with
  | Resp.Integer count -> return @@ `Count count
  | Resp.Array sorted ->
      sorted
      |> List.map ~f:(function
             | Resp.Bulk v -> Ok v
             | _ -> Error `Unexpected )
      |> Result.all
      |> Result.map ~f:(fun x -> `Sorted x)
      |> Deferred.return
  | _ -> Deferred.return @@ Error `Unexpected

let ttl t key =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["PTTL"; key] with
  | Resp.Integer -2 -> Deferred.return @@ Error (`No_such_key key)
  | Resp.Integer -1 -> Deferred.return @@ Error (`Not_expiring key)
  | Resp.Integer ms -> ms |> float_of_int |> Time.Span.of_ms |> return
  | _ -> Deferred.return @@ Error `Unexpected

let type' t key =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["TYPE"; key] with
  | Resp.String "none" -> return None
  | Resp.String s -> return @@ Some s
  | _ -> Deferred.return @@ Error `Unexpected

let dump t key =
  let open Deferred.Result.Let_syntax in
  match%bind request t ["DUMP"; key] with
  | Resp.Bulk bulk -> return @@ Some bulk
  | Resp.Null -> return None
  | _ -> Deferred.return @@ Error `Unexpected

let restore t ~key ?ttl ?replace value =
  let open Deferred.Result.Let_syntax in
  let ttl =
    match ttl with
    | None -> "0"
    | Some span -> span |> Time.Span.to_ms |> Printf.sprintf ".0%f"
  in
  let replace =
    match replace with
    | Some true -> ["REPLACE"]
    | Some false | None -> []
  in
  match%bind request t (["RESTORE"; key; ttl; value] @ replace) with
  | Resp.String "OK" -> return ()
  | _ -> Deferred.return @@ Error `Unexpected

let with_connection ?(port = 6379) ~host f =
  let where =
    Tcp.Where_to_connect.of_host_and_port @@ Host_and_port.create ~host ~port
  in
  Tcp.with_connection where @@ fun _socket reader writer ->
  let t = init reader writer in
  f t
