module Config = struct
  type t =
    { recording_dir : string option
    }

  let create_dir_if_needed dir =
    match Sys.is_directory dir with
    | true -> Some dir
    | false -> None
    | exception Sys_error _ -> Unix.mkdir dir 0o755; Some dir

  let make ?recording_dir () =
    let open CCOpt.Infix in
    { recording_dir = recording_dir >>= create_dir_if_needed
    }

  let from_env () =
    make
      ?recording_dir:(Sys.getenv_opt "STITCH_OCAML_RECORDING_DIR")
      ()

  let recording_file t name =
    let filename = Printf.sprintf "%s.json" name in
    CCOpt.map (fun dir -> Filename.concat dir filename) t.recording_dir
end

module Recorded_call = struct
  type t =
    { arguments : Yojson.Safe.json list
    ; return_value : Yojson.Safe.json
    ; execution_time : float
    }
  [@@deriving yojson]
end

module Function = struct
  module Argument = struct
    type 'a t =
      { to_yojson : 'a -> Yojson.Safe.json
      ; of_yojson : Yojson.Safe.json -> ('a, string) result
      }
    [@@deriving make]

    let serialize t a = t.to_yojson a
    let deserialize t json = t.of_yojson json
  end

  module Return_value = struct
    type 'a t =
      { to_yojson : 'a -> Yojson.Safe.json
      ; of_yojson : Yojson.Safe.json -> ('a, string) result
      ; equal : 'a -> 'a -> bool
      ; show : 'a -> string
      }
    [@@deriving make]

    let serialize t a = t.to_yojson a
    let deserialize t json = t.of_yojson json
    let equal t a a' = t.equal a a'
    let show t a = t.show a
  end

  module Arity_2 = struct
    type ('a, 'b, 'ret) t =
      { arg_a : 'a Argument.t
      ; arg_b : 'b Argument.t
      ; return_value : 'ret Return_value.t
      }
    [@@deriving make]

    let equal_return_value {return_value; _} rv rv' = Return_value.equal return_value rv rv'
    let show_return_value {return_value; _} rv = Return_value.show return_value rv

    let record t f a b =
      let serialized_a = Argument.serialize t.arg_a a in
      let serialized_b = Argument.serialize t.arg_b b in
      let start = Unix.gettimeofday () in
      let rv = f a b in
      let stop = Unix.gettimeofday () in
      let serialized_rv = Return_value.serialize t.return_value rv in
      let recorded_call =
        { Recorded_call.arguments = [serialized_a; serialized_b]
        ; return_value = serialized_rv
        ; execution_time = stop -. start
        }
      in
      (rv, recorded_call)

    let replay t f {Recorded_call.arguments; return_value; _} =
      match arguments with
      | [serialized_a; serialized_b]
        ->
        let open CCResult in
        Argument.deserialize t.arg_a serialized_a >>= fun a ->
        Argument.deserialize t.arg_b serialized_b >>= fun b ->
        Return_value.deserialize t.return_value return_value >|= fun rv ->
        (rv, f a b)
      | _
        ->
        Error "Recorded call and seam signature don't match"
  end

  module Arity_3 = struct
    type ('a, 'b, 'c, 'ret) t =
      { arg_a : 'a Argument.t
      ; arg_b : 'b Argument.t
      ; arg_c : 'c Argument.t
      ; return_value : 'ret Return_value.t
      }
    [@@deriving make]

    let equal_return_value {return_value; _} rv rv' = Return_value.equal return_value rv rv'
    let show_return_value {return_value; _} rv = Return_value.show return_value rv

    let record t f a b c =
      let serialized_a = Argument.serialize t.arg_a a in
      let serialized_b = Argument.serialize t.arg_b b in
      let serialized_c = Argument.serialize t.arg_c c in
      let start = Unix.gettimeofday () in
      let rv = f a b c in
      let stop = Unix.gettimeofday () in
      let serialized_rv = Return_value.serialize t.return_value rv in
      let recorded_call =
        { Recorded_call.arguments = [serialized_a; serialized_b; serialized_c]
        ; return_value = serialized_rv
        ; execution_time = stop -. start
        }
      in
      (rv, recorded_call)

    let replay t f {Recorded_call.arguments; return_value; _} =
      match arguments with
      | [serialized_a; serialized_b; serialized_c]
        ->
        let open CCResult in
        Argument.deserialize t.arg_a serialized_a >>= fun a ->
        Argument.deserialize t.arg_b serialized_b >>= fun b ->
        Argument.deserialize t.arg_c serialized_c >>= fun c ->
        Return_value.deserialize t.return_value return_value >|= fun rv ->
        (rv, f a b c)
      | _
        ->
        Error "Recorded call and seam signature don't match"
  end
end

let stream_mapi ~f stream =
  let gen i =
    let next =
      let open CCOpt.Infix in
      Stream.peek stream >|= fun v -> f (i, v)
    in
    Stream.junk stream;
    next
  in
  Stream.from gen

let stream_map ~f stream = stream_mapi ~f:(fun (_i, v) -> f v) stream

let stream_to_list stream =
  let l = ref [] in
  Stream.iter (fun v -> l := v::!l) stream;
  List.rev !l

let append ~recording_file ~recorded_call =
  let channel = open_out_gen [Open_creat; Open_append] 0o644 recording_file in
  Yojson.Safe.to_channel channel @@ Recorded_call.to_yojson recorded_call;
  close_out channel

let recorded_call_stream file =
  let json_stream = Yojson.Safe.stream_from_file file in
  stream_map ~f:Recorded_call.of_yojson json_stream

let recorded_function_2 ~signature ~f recording_file =
  fun a b ->
    let (rv, recorded_call) = Function.Arity_2.record signature f a b in
    append ~recording_file ~recorded_call;
    rv

let recorded_function_3 ~signature ~f recording_file =
  fun a b c ->
    let (rv, recorded_call) = Function.Arity_3.record signature f a b c in
    append ~recording_file ~recorded_call;
    rv

let create_2 ~config ~name ~signature f =
  let recording_file = Config.recording_file config name in
  CCOpt.map_or ~default:f (recorded_function_2 ~signature ~f) recording_file

let create_3 ~config ~name ~signature f =
  let recording_file = Config.recording_file config name in
  CCOpt.map_or ~default:f (recorded_function_3 ~signature ~f) recording_file

let verify ~test_of_recorded_call recording_file =
  let open OUnit2 in
  let recorded_call_stream = recorded_call_stream recording_file in
  let parsing_failure msg _ctxt = assert_failure @@ Printf.sprintf "Parsing error: %s" msg in
  let test_stream =
    stream_mapi
      ~f:
        ( function
          | (i, Ok rc) -> string_of_int i >:: test_of_recorded_call rc
          | (i, Error msg) -> string_of_int i >:: parsing_failure msg
        )
      recorded_call_stream
  in
  stream_to_list test_stream

let test_list ~config ~name ~test_of_recorded_call =
  let recording_file = Config.recording_file config name in
  CCOpt.map_or ~default:[] (verify ~test_of_recorded_call) recording_file

let verify_2 ~config ~name ~signature f =
  let test_of_recorded_call recorded_call ctxt =
    let open OUnit2 in
    let cmp = Function.Arity_2.equal_return_value signature in
    let printer = Function.Arity_2.show_return_value signature in
    match Function.Arity_2.replay signature f recorded_call with
    | Ok (expected, actual) -> assert_equal ~ctxt ~cmp ~printer expected actual
    | Error msg -> assert_failure msg
  in
  test_list ~config ~name ~test_of_recorded_call

let verify_3 ~config ~name ~signature f =
  let test_of_recorded_call recorded_call ctxt =
    let open OUnit2 in
    let cmp = Function.Arity_3.equal_return_value signature in
    let printer = Function.Arity_3.show_return_value signature in
    match Function.Arity_3.replay signature f recorded_call with
    | Ok (expected, actual) -> assert_equal ~ctxt ~cmp ~printer expected actual
    | Error msg -> assert_failure msg
  in
  test_list ~config ~name ~test_of_recorded_call
