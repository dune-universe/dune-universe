val syslog_report_common :
  Syslog_message.facility option -> string -> int -> (unit -> Ptime.t) ->
  (string -> unit Lwt.t) -> (?len:int -> Syslog_message.t -> string) ->
  Logs.reporter
