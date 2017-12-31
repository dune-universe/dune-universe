open Core
open Core_profiler_offline_tool.Std

let rec name id_map id =
  match Reader.Header.find_exn id_map id with
  | Single { name=n; _ } -> n
  | Group { name=n; _ } -> n
  | Group_point { name=n; parent; sources=_ } ->
    (name id_map parent) ^ "." ^ n

let display_message (m : Reader.Short_message.t) id_map =
  match m with
  | Timer (id, time) ->
    printf !"%{Time_ns} %-20s mark\n" time (name id_map id)
  | Probe (id, time, value) ->
    printf !"%{Time_ns} %-20s %i\n" time (name id_map id) value
  | Group_reset (id, time) ->
    printf !"%{Time_ns} %-20s group reset\n" time (name id_map id)

let main buffer () =
  let (epoch, id_map) = Reader.consume_header buffer in
  Reader.iter_short_messages buffer epoch id_map ~f:(fun message ->
    display_message message id_map
  )

let command =
  Command.basic_spec
    ~summary:"Dump the contents of a Core_profiler.Offline file"
    Command.Spec.(
      let iobuf_file_arg_type = Arg_type.create Reader.map_file in

      empty
      +> anon ("filename" %: iobuf_file_arg_type)
    )
    main

let () =
  Command.run command
