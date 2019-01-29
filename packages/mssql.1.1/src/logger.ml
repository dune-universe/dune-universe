open Core
open Async

let src = Logs.Src.create "mssql"

let lib_tag =
  Logs.Tag.def "lib" Format.pp_print_string

let msg ?(tags=Logs.Tag.empty) level fmt =
  ksprintf (fun msg ->
    Logs.msg ~src level (fun m ->
      let tags = Logs.Tag.add lib_tag "mssql" tags in
      m ~tags "%s" msg))
    fmt

let debug ?tags fmt =
  msg ?tags Logs.Debug fmt

let debug_in_thread ?tags fmt =
  Thread_safe.run_in_async_exn (fun () ->
    debug ?tags fmt)

let info ?tags fmt =
  msg ?tags Logs.Info fmt

let info_in_thread ?tags fmt =
  Thread_safe.run_in_async_exn (fun () ->
    info ?tags fmt)

let error ?tags fmt =
  msg ?tags Logs.Error fmt

let error_in_thread ?tags fmt =
  Thread_safe.run_in_async_exn (fun () ->
    error ?tags fmt)
