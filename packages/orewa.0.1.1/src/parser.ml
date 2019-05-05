open Core
open Async

let record_length iobuf =
  let cr = '\r' in
  let lf = '\n' in
  match Iobuf.Peek.index iobuf cr, Iobuf.Peek.index iobuf lf with
  | Some cr_index, Some lf_index when lf_index = Int.succ cr_index ->
      Some (Int.succ lf_index)
  | _ -> None

let consume_record ~len iobuf =
  Iobuf.Consume.stringo ~len iobuf |> String.subo ~len:(len - 2)

let discard_prefix = String.subo ~pos:1

type nested_resp =
  | Atomic of Resp.t
  | Array of (int * nested_resp Stack.t)
  | String of (int * string list)

let rec show_nested_resp = function
  | Atomic _ -> "Element(..)"
  | Array (to_read, stack) ->
      let v =
        stack |> Stack.to_list |> List.map ~f:show_nested_resp |> String.concat ~sep:"; "
      in
      Printf.sprintf "Array[%d](%s)" to_read v
  | String (left, _) -> Printf.sprintf "String[%d](..)" left

let rec unfinished_array stack =
  match Stack.top stack with
  | Some (Array (0, stack)) -> unfinished_array stack
  | Some (Array (n, _)) when n > 0 -> true
  | _ -> false

let rec one_record_read stack e =
  match Stack.top stack with
  | None | Some (Atomic _) | Some (String _) -> Stack.push stack e
  | Some (Array (0, substack)) -> (
    match unfinished_array substack with
    | false -> Stack.push stack e
    | true -> one_record_read substack e )
  | Some (Array (left_to_read, inner_stack)) ->
      let _ = Stack.pop stack in
      one_record_read inner_stack e;
      let updated = Array (Int.pred left_to_read, inner_stack) in
      Stack.push stack updated

let rec update_latest_bulk_read stack retrieved s =
  match Stack.top stack with
  | Some (String (0, _)) -> ()
  | Some (Array (_, stack)) -> update_latest_bulk_read stack retrieved s
  | Some (String (left_to_read, rope)) ->
      let _ = Stack.pop stack in
      let left_to_read = left_to_read - retrieved in
      ( match left_to_read with
      | 0 -> Log.Global.debug "Bulk read finished"
      | _ -> () );
      let updated = String (left_to_read, s :: rope) in
      Stack.push stack updated
  | _ -> ()

let rec unwind_stack stack =
  stack
  |> Stack.to_list
  |> List.map ~f:(function
         | Atomic resp -> resp
         | String (_, rope) ->
             let s = rope |> List.rev |> String.concat in
             Resp.Bulk (String.subo ~len:(String.length s - 2) s)
         | Array (_, stack) -> Resp.Array (List.rev (unwind_stack stack)) )

let rec bulk_left_to_read stack =
  match Stack.top stack with
  | Some (Array (_, inner_stack)) -> bulk_left_to_read inner_stack
  | Some (String (bytes, _)) when bytes > 0 -> Some bytes
  | _ -> None

let rec handle_chunk stack iobuf =
  let read_on stack =
    match unfinished_array stack with
    | true -> handle_chunk stack iobuf
    | false -> return @@ `Stop ()
  in
  let read_simple_string ~len constructor =
    let content = consume_record ~len iobuf |> discard_prefix in
    Log.Global.debug "READ: %s" (String.escaped content);
    one_record_read stack (Atomic (constructor content));
    read_on stack
  in
  match bulk_left_to_read stack with
  | Some left_to_read -> (
      Log.Global.debug
        "There is some bulk to read, current stack: %s"
        ( stack
        |> Stack.to_list
        |> List.map ~f:show_nested_resp
        |> String.concat ~sep:"; " );
      let retrieved = Iobuf.length iobuf in
      Log.Global.debug
        "Bulk left to read: %d bytes, retrieved %d bytes"
        left_to_read
        retrieved;
      match left_to_read <= retrieved with
      | true ->
          let content = Iobuf.Consume.stringo ~len:left_to_read iobuf in
          update_latest_bulk_read stack left_to_read content;
          read_on stack
      | false ->
          let content = Iobuf.Consume.stringo ~len:retrieved iobuf in
          update_latest_bulk_read stack retrieved content;
          return `Continue )
  | None -> (
    match record_length iobuf with
    | None -> return `Continue
    | Some len -> (
      (* peek, because if `Continue is returned we need to preserve the prefix and
       * don't consume it *)
      match Iobuf.Peek.char ~pos:0 iobuf with
      | '+' ->
          (* Simple string *)
          read_simple_string ~len (fun content -> Resp.String content)
      | '-' ->
          (* Error, which is also a simple string *)
          read_simple_string ~len (fun content -> Resp.Error content)
      | '$' -> (
          (* Bulk string *)
          let bulk_len = consume_record ~len iobuf |> discard_prefix |> int_of_string in
          match bulk_len = -1 with
          | true ->
              (* bulk length -1 means it is nil *)
              let resp = Resp.Null in
              one_record_read stack (Atomic resp);
              read_on stack
          | false -> (
              let retrieved = Iobuf.length iobuf in
              match bulk_len <= retrieved with
              | true ->
                  (* read including the trailing \r\n and discard those *)
                  let content = consume_record ~len:(bulk_len + 2) iobuf in
                  let resp = Resp.Bulk content in
                  one_record_read stack (Atomic resp);
                  read_on stack
              | false ->
                  let content = Iobuf.Consume.stringo ~len:retrieved iobuf in
                  let left_to_read = bulk_len - retrieved + 2 in
                  one_record_read stack (String (left_to_read, [content]));
                  return @@ `Continue ) )
      | ':' ->
          (* Integer *)
          let value = consume_record ~len iobuf |> discard_prefix |> int_of_string in
          let resp = Resp.Integer value in
          one_record_read stack (Atomic resp);
          read_on stack
      | '*' -> (
          (* Array *)
          let elements = consume_record ~len iobuf |> discard_prefix |> int_of_string in
          Log.Global.debug "Array of %d to be read" elements;
          match elements with
          | 0 ->
              (* empty arrays just end after the <elements>\r\n, do not expect more args *)
              let resp = Resp.Array [] in
              one_record_read stack (Atomic resp);
              read_on stack
          | elements ->
              one_record_read stack (Array (elements, Stack.create ()));
              handle_chunk stack iobuf )
      | unknown ->
          (* Unknown match *)
          Log.Global.debug "Unparseable type tag %C" unknown;
          Log.Global.debug
            "Failing stack: %s"
            ( stack
            |> Stack.to_list
            |> List.map ~f:show_nested_resp
            |> String.concat ~sep:"; " );
          return @@ `Stop () ) )

type resp_list = Resp.t list [@@deriving show]

let read_resp reader =
  let stack = Stack.create () in
  let%bind res =
    Reader.read_one_iobuf_at_a_time reader ~handle_chunk:(handle_chunk stack)
  in
  Log.Global.debug
    "Stack is %s "
    (stack |> Stack.to_list |> List.map ~f:show_nested_resp |> String.concat ~sep:", ");
  let resps = unwind_stack stack in
  Log.Global.debug "Stack unwound to: %s" (show_resp_list resps);
  match List.hd resps with
  | None -> return @@ Error `Unexpected
  | Some resp -> (
    match res with
    | `Eof -> return @@ Error `Eof
    | `Stopped () -> return @@ Ok resp
    | `Eof_with_unconsumed_data _data -> return @@ Error `Connection_closed )
