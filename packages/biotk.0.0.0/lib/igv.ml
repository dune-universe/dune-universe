open Core_kernel

module Statement = struct
  type t =
    | New
    | Genome of string
    | Load of {
        format : string option ;
        index : string option ;
        name : string option ;
        path_or_url : string ;
      }

  let _new_ = New
  let genome g = Genome g
  let load ?format ?index ?name path_or_url = Load { format ; index ; path_or_url ; name }
  let to_string = function
    | New -> "new"
    | Genome g -> sprintf "genome %s" g
    | Load l ->
      let tag label = Option.value_map ~default:"" ~f:(fun value ->
          sprintf " %s=%s" label value
        )
      in
      sprintf "load %s%s%s%s"
        l.path_or_url
        (tag "index" l.index)
        (tag "format" l.format)
        (tag "name" l.name)
end

let script_of_statements xs =
  List.map xs ~f:Statement.to_string
  |> String.concat ~sep:"\n"

let send_command ic oc c =
  Out_channel.output_string oc (Statement.to_string c) ;
  Out_channel.output_char oc '\n' ;
  Out_channel.flush oc ;
  match In_channel.input_line ic with
  | Some "OK" -> Ok ()
  | None -> Error "Connection lost!"
  | Some msg -> Error msg

class proxy ?(addr = "127.0.0.1") ?(port = 60151) () =
  let ic, oc = Core.Unix.(open_connection (ADDR_INET (Inet_addr.of_string addr, port))) in
  let send = send_command ic oc in
  object
    method _new_ = send New
    method genome g = send (Genome g)
    method load ?format ?index ?name path_or_url = send (Statement.load ?format ?index ?name path_or_url)
  end
