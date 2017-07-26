open Core
open Core_profiler_disabled

let debug = false

let default_output_filename = "profiler.dat"

let current_output_filename =
  let env = "OUTPUT_FILE" in
  let v =
    match Check_environment.get_var env with
    | Some v -> v
    | None -> default_output_filename
  in
  if debug then printf "file = %s\n" v;
  ref v

let set_current_output_filename v =
  current_output_filename := v


module Short_header : sig
  val id_bits : int
  val time_bits : int

  val max_id : int
  val max_time_diff : Time_ns.Span.t

  val pack_exn    : Profiler_epoch.t -> Probe_id.t -> Time_ns.t -> int
  val pack_unsafe : Profiler_epoch.t -> Probe_id.t -> Time_ns.t -> int
  val unpack_id   : int -> Probe_id.t
  val unpack_time : Profiler_epoch.t -> int -> Time_ns.t
  val unpack      : Profiler_epoch.t -> int -> Probe_id.t * Time_ns.t
end = struct
  let time_bits = 54            (* ~208 days *)
  let%test _ = time_bits > 1
  let%test _ = time_bits < 63

  let id_bits = 63 - time_bits  (* you may need to change the size of header_chunk *)

  let max_id = 1 lsl id_bits - 1
  let max_time_diff_int = 1 lsl time_bits - 1
  let max_time_diff = Time_ns.Span.of_int_ns max_time_diff_int

  let pack_exn epoch id time =
    let id = Probe_id.to_int_exn id in
    let time =
      Profiler_epoch.diff epoch time
      |> Time_ns.Span.to_int_ns
    in
    if (   time < 0
        || time > max_time_diff_int
        || id < 0
        || id > max_id)
    then
      raise (Invalid_argument "parameter out of range")
    else
      time lor (id lsl time_bits)

  let pack_unsafe epoch id time =
    let id = Probe_id.to_int_exn id in
    let time =
      Profiler_epoch.diff epoch time
      |> Time_ns.Span.to_int_ns
    in
    (time land (1 lsl time_bits - 1)) lor (id lsl time_bits)

  let unpack_id header =
    header lsr time_bits |> Probe_id.of_int_exn

  let unpack_time epoch header =
    header land (1 lsl time_bits - 1)
    |> Time_ns.Span.of_int_ns
    |> Profiler_epoch.add epoch

  let unpack epoch header =
    ( unpack_id header
    , unpack_time epoch header
    )

  let%test_module "unpack_pack" = (module struct
    let epoch =
      Profiler_epoch.of_time
        (Time_ns.of_int_ns_since_epoch (Int64.to_int_exn 1405085600000000000L))

    let test id time =
      let id = Probe_id.of_int_exn id in
      let time = Time_ns.of_int_ns_since_epoch (Int64.to_int_exn time) in

      let packed = pack_exn epoch id time in
      let packed_unsafe = pack_unsafe epoch id time in
      let unpacked = unpack epoch packed_unsafe in

      [%test_eq: int] packed packed_unsafe;
      [%test_eq: Probe_id.t * Time_ns.t] unpacked (id, time)

    let%test_unit "0 0"         = test 0    1405085600000000000L
    let%test_unit "max max"     = test 511  1423099998509481983L
    let%test_unit "1 1"         = test 1    1405085600000000001L
    let%test_unit "256 100_000" = test 256  1405085600000100000L
  end)

  let%bench_module "Short message header packing" = (module struct
    let epoch =
      Profiler_epoch.of_time
        (Time_ns.of_int_ns_since_epoch (Int64.to_int_exn 1405085600000000000L))
    let id = Probe_id.of_int_exn 123
    let time = Time_ns.of_int_ns_since_epoch (Int64.to_int_exn 1405085600123123000L)

    let%bench "pack_exn"     = ignore (pack_exn     epoch id time : int)
    let%bench "pack_unsafe"  = ignore (pack_unsafe  epoch id time : int)
  end)
end

module Buffer : sig
  (* read_write buffers are exposed to Writer *)
  val header_chunk  : (read_write, _) Iobuf.t Lazy.t
  val current_chunk : (read_write, _) Iobuf.t

  (* These are public: *)

  (** Is the main (short message) buffer empty? *)
  val is_empty : unit -> bool

  val get_chunks : unit -> (read_write, Iobuf.no_seek) Iobuf.t list
  val get_header_chunk : unit -> (read, _) Iobuf.t

  val ensure_free : int -> unit

  module Unsafe_internals : sig
    val reset : unit -> unit
  end
end = struct
  (* If we create 512 group points with every other point as a source,
     this buffer _still_ won't fill up  (512 * (72 + 512 * 2)) *)
  let header_chunk = lazy (Iobuf.create ~len:(561152))

  let get_header_chunk () =
    let copy = Iobuf.create ~len:0 in

    if Lazy.is_val header_chunk
    then begin
      Iobuf.set_bounds_and_buffer ~src:(Lazy.force header_chunk) ~dst:copy;
      Iobuf.flip_lo copy
    end;

    (copy :> (read, _) Iobuf.t)

  (* Iobufs are mutable to the extent that you can swap the pointer to the underlying
     memory with another Iobuf. I use this to avoid a [ref] / another indirection:
     When we want to swap the buffer, we copy its pointer & limits into a new [Iobuf.t]
     structure, and then overwrite it with the pointer & limits from a freshly created
     [Iobuf.t] *)
  let current_chunk = Iobuf.create ~len:0
  let chunk_size = 10_000_000
  let previous_chunks = ref []

  let allocate_new_chunk len =
    Iobuf.flip_lo current_chunk;

    if not (Iobuf.is_empty current_chunk) then begin
      (* Use sub to copy the Iobuf.t structure (and narrow the chunk in the process). *)
      let copy = Iobuf.sub_shared current_chunk in
      previous_chunks := copy :: !previous_chunks
    end;

    let new_memory = Iobuf.create ~len in
    Iobuf.set_bounds_and_buffer ~src:new_memory ~dst:current_chunk;

    (* We need to force the kernel to actually give us the memory, or we're liable to
       get spikes in poke times. *)
    if len > 0
    then
      for i = 0 to (len - 1) / 512 do
        Iobuf.Unsafe.Poke.uint8 current_chunk ~pos:(i * 512) 0
      done

  let ensure_free len =
    assert (len <= chunk_size);
    if Iobuf.length current_chunk < len then allocate_new_chunk chunk_size

  let get_chunks () =
    (* ... thereby moving the curent chunk into [previous_chunks] *)
    allocate_new_chunk 0;
    List.rev !previous_chunks

  let is_empty () = List.is_empty (get_chunks ())

  module Unsafe_internals = struct
    let reset () =
      if Lazy.is_val header_chunk then Iobuf.reset (Lazy.force header_chunk);
      allocate_new_chunk 0;
      previous_chunks := []
  end

  let%test_unit "allocate_new_chunk" =
    protect
      ~f:(fun () ->
        allocate_new_chunk 1000;
        [%test_eq: int] (Iobuf.length current_chunk) 1000;
        Iobuf.Fill.stringo current_chunk "the first chunk\n";
        Iobuf.Fill.stringo current_chunk "still the first chunk\n";

        allocate_new_chunk 500;
        allocate_new_chunk 500; (* empty, should be ignored *)
        Iobuf.Fill.stringo current_chunk "the second chunk\n";

        allocate_new_chunk 0;
        [%test_eq: int] (Iobuf.length current_chunk) 0;

        [%test_eq: int] (List.length !previous_chunks) 2;

        begin
          match !previous_chunks with
          | [second; first] ->
            [%test_eq: string]
              (Iobuf.to_string first)
              "the first chunk\nstill the first chunk\n";
            [%test_eq: string]
              (Iobuf.to_string second)
              "the second chunk\n"
          | _ -> assert false
        end
      )
      ~finally:Unsafe_internals.reset

  let%test_unit "ensure_free" =
    protect
      ~f:(fun () ->
        ensure_free 100;
        [%test_eq: int] (Iobuf.length current_chunk) chunk_size;
        [%test_eq: int] (List.length !previous_chunks) 0;

        Iobuf.advance current_chunk (chunk_size - 50);
        ensure_free 500;
        [%test_eq: int] (Iobuf.length current_chunk) chunk_size;
        [%test_eq: int] (List.length !previous_chunks) 1;
      )
      ~finally:Unsafe_internals.reset

  let%test_unit "get_header_chunk" =
    protect
      ~f:(fun () ->
        let header_chunk = Lazy.force header_chunk in
        Iobuf.Fill.stringo header_chunk "some data";
        let contents =
          get_header_chunk ()
          |> Iobuf.to_string
        in
        [%test_eq: string] contents "some data"
      )
      ~finally:Unsafe_internals.reset

  let%test_unit "get_chunks" =
    protect
      ~f:(fun () ->
        allocate_new_chunk 1000;
        Iobuf.Fill.stringo current_chunk "the first chunk";
        allocate_new_chunk 1000;
        Iobuf.Fill.stringo current_chunk "the second chunk";
        let contents =
          get_chunks ()
          |> List.map ~f:Iobuf.to_string
        in
        [%test_eq: string list] contents ["the first chunk"; "the second chunk"]
      )
      ~finally:Unsafe_internals.reset
end

module Writer = struct
  let epoch =
    Time_ns.now ()
    |> Fn.flip Time_ns.sub (Time_ns.Span.of_min 1.)
    |> Profiler_epoch.of_time

  let max_time = Profiler_epoch.add epoch Short_header.max_time_diff

  let write_epoch () =
    let header_chunk = Lazy.force Buffer.header_chunk in
    let written = Header_protocol.Epoch.write ~epoch header_chunk in
    Iobuf.advance header_chunk written

  let write_end_of_header () =
    let header_chunk = Lazy.force Buffer.header_chunk in
    let written = Header_protocol.End_of_header.write header_chunk in
    Iobuf.advance header_chunk written

  let write_new_single id name spec =
    let header_chunk = Lazy.force Buffer.header_chunk in

    let written =
      Header_protocol.New_single.write
        ~id ~spec ~name header_chunk
    in
    Iobuf.advance header_chunk written

  let write_new_group id name spec =
    let header_chunk = Lazy.force Buffer.header_chunk in

    let written =
      Header_protocol.New_group.write
        ~id ~spec ~name header_chunk
    in
    Iobuf.advance header_chunk written

  let write_new_group_point ~group_id ~id name sources =
    let header_chunk = Lazy.force Buffer.header_chunk in
    let module NPP = Header_protocol.New_group_point in

    let sources_count = Array.length sources in
    let len = NPP.write ~group_id ~id ~name ~sources_count header_chunk in
    Array.iteri sources ~f:(fun index id ->
      NPP.write_sources header_chunk ~count:sources_count ~index ~source_id:id
    );
    Iobuf.advance header_chunk len

  let write_timer_at id time =
    Buffer.ensure_free 8;
    Iobuf.Unsafe.Fill.int64_le
      Buffer.current_chunk
      (Short_header.pack_unsafe epoch id time)

  let write_probe_at id time value =
    let current_chunk = Buffer.current_chunk in
    Buffer.ensure_free 16;
    Iobuf.Unsafe.Poke.int64_le
      current_chunk ~pos:0
      (Short_header.pack_unsafe epoch id time);
    Iobuf.Unsafe.Poke.int64_le
      current_chunk ~pos:8
      value;
    Iobuf.unsafe_advance current_chunk 16

  let write_group_reset = write_timer_at

  let%test_module "write header messages" = (module struct
    let unpack_one () =
      let chunk = Lazy.force Buffer.header_chunk in
      Iobuf.flip_lo chunk;

      match Header_protocol.to_unpacked chunk with
      | Ok (unpacked, length) ->
        [%test_eq: int] (Iobuf.length chunk) length;
        unpacked
      | _ ->
        failwith "to_unpacked failed"

    let%test_unit "write_new_single" =
      protect
        ~finally:Buffer.Unsafe_internals.reset
        ~f:(fun () ->
          write_new_single
            (Probe_id.of_int_exn 100)
            "unittest"
            (Probe_type.Timer);
          match unpack_one () with
          | New_single { id; spec; name; message_length=_; message_type=_ } ->
            [%test_eq: Probe_id.t] id (Probe_id.of_int_exn 100);
            [%test_eq: Probe_type.t] spec Probe_type.Timer;
            [%test_eq: string] name "unittest"
          | _ -> failwith "Incorrect message type"
        )

    let%test_unit "write_new_group" =
      protect
        ~finally:Buffer.Unsafe_internals.reset
        ~f:(fun () ->
          write_new_group
            (Probe_id.of_int_exn 100)
            "unittest"
            (Probe_type.Probe Profiler_units.Seconds);
          match (unpack_one ()) with
          | New_group { id; spec; name; message_length=_; message_type=_ } ->
            [%test_eq: Probe_id.t] id (Probe_id.of_int_exn 100);
            [%test_eq: Probe_type.t] spec (Probe_type.Probe Profiler_units.Seconds);
            [%test_eq: string] name "unittest"
          | _ -> failwith "Incorrect message type"
        )

    let%test_unit "write_new_group_point" =
      protect
        ~finally:Buffer.Unsafe_internals.reset
        ~f:(fun () ->
          write_new_group_point
            ~group_id:(Probe_id.of_int_exn 100)
            ~id:(Probe_id.of_int_exn 300)
            "unittest"
            (Array.map ~f:Probe_id.of_int_exn [|500; 700|]);
          match unpack_one () with
          | New_group_point
              { group_id; id; name; sources_grp; message_length=_; message_type=_ } ->
            [%test_eq: int] (Probe_id.to_int_exn group_id) 100;
            [%test_eq: int] (Probe_id.to_int_exn id) 300;
            [%test_eq: string] name "unittest";
            [%test_eq: int array]
              (Array.map sources_grp ~f:(fun r ->
                 let r : Header_protocol.New_group_point.Unpacked.t_sources = r in
                 Probe_id.to_int_exn r.source_id))
              [|500; 700|];
          | _ ->
            assert false
        )
  end)

  let write_to_fd fd header_chunk chunks =
    List.iter
      (header_chunk :: chunks)
      ~f:(fun chunk ->
        Iobuf.protect_window_and_bounds chunk ~f:(fun chunk ->
          Bigstring.really_write fd (Iobuf.Peek.bigstringo ~pos:0 chunk)

        )
      )

  let%test_unit "write_to_fd" =
    let (filename, fd) = Unix.mkstemp "/tmp/core-profiler-tests" in
    protect
      ~f:(fun () ->
        let header_chunk = Iobuf.of_string "the header chunk\n" in
        let chunks =
          [ Iobuf.of_string "the first chunk\n"
          ; Iobuf.of_string "the second chunk\n"
          ]
        in

        write_to_fd fd header_chunk chunks;
        Unix.close fd;

        [%test_eq: string]
          (In_channel.read_all filename)
          "the header chunk\nthe first chunk\nthe second chunk\n";
      )
      ~finally:(fun () ->
        begin
          try Unix.close fd
          with _ -> ()
        end;
        Unix.unlink filename;
      )

  let write_to_file name_ref header_chunk chunks =
    let name = !name_ref in
    begin
      match Sys.file_exists name with
      | `Yes -> Unix.rename ~src:name ~dst:(name ^ ".old")
      | `No | `Unknown -> ()
    end;

    Unix.with_file
      name
      ~mode:[ Unix.O_CREAT; Unix.O_WRONLY; Unix.O_TRUNC ]
      ~f:(fun fd ->
        write_to_fd fd header_chunk chunks
      )

  let at_exit_handler = ref (Some (write_to_file current_output_filename))

  let set_at_exit_handler = function
    | `Write_file name -> at_exit_handler := Some (write_to_file (ref name))
    | `Function f ->      at_exit_handler := Some f
    | `Disable ->         at_exit_handler := None

  let dump_stats_internal handler =
    write_epoch ();
    write_end_of_header ();

    let chunks = Buffer.get_chunks () in

    if not (List.is_empty chunks)
    then handler (Buffer.get_header_chunk ())
           (chunks :> (read, Iobuf.no_seek) Iobuf.t list)

  let dump_stats () =
    Option.iter !at_exit_handler ~f:(fun handler ->
      let header_chunk = Lazy.force Buffer.header_chunk in
      let lo_bound = Iobuf.Lo_bound.window header_chunk in
      let hi_bound = Iobuf.Hi_bound.window header_chunk in
      dump_stats_internal handler;
      Iobuf.Lo_bound.restore lo_bound header_chunk;
      Iobuf.Hi_bound.restore hi_bound header_chunk)

  let () = at_exit (fun () ->
    Option.iter !at_exit_handler ~f:dump_stats_internal
  )

  module Unsafe_internals = struct
    let write_epoch = write_epoch
    let write_end_of_header = write_end_of_header
  end
end
