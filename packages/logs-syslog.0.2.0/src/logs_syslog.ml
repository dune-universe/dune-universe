
let slevel = function
  | Logs.App -> Syslog_message.Informational
  | Logs.Error -> Syslog_message.Error
  | Logs.Warning -> Syslog_message.Warning
  | Logs.Info -> Syslog_message.Informational
  | Logs.Debug -> Syslog_message.Debug

let ppf, flush =
  let b = Buffer.create 255 in
  let ppf = Format.formatter_of_buffer b in
  let flush () =
    Format.pp_print_flush ppf () ;
    let s = Buffer.contents b in Buffer.clear b ; s
  in
  ppf, flush

let facility =
  let ppf ppf v =
    Format.pp_print_string ppf (Syslog_message.string_of_facility v)
  in
  Logs.Tag.def ~doc:"Syslog facility" "syslog-facility" ppf

let message ?facility:(syslog_facility = Syslog_message.System_Daemons)
    ~host:hostname ~source ~tags ?header level timestamp message =
  let tags =
    let tags = Logs.Tag.rem facility tags in
    if Logs.Tag.is_empty tags then
      ""
    else
      (Logs.Tag.pp_set ppf tags ;
       " " ^ flush ())
  in
  let hdr = match header with None -> "" | Some x -> " " ^ x in
  let content = Printf.sprintf "%s%s %s" tags hdr message
  and severity = slevel level
  and tag = Astring.String.take ~max:32 source
  in
  { Syslog_message.facility = syslog_facility ; severity ; timestamp ;
                   hostname ; tag ; content }

type framing = [
  | `LineFeed
  | `Null
  | `Custom of string
  | `Count
]

let frame_message msg = function
  | `LineFeed -> msg ^ "\n"
  | `Null -> msg ^ "\000"
  | `Custom s -> msg ^ s
  | `Count -> Printf.sprintf "%d %s" (String.length msg) msg
