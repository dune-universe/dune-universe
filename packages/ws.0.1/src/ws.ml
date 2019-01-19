open CCFun

type state =
  (*| Connecting *)
  | Open
  | Closed

let lowercase_header headers header = List.assoc_opt header headers |> CCOpt.map String.lowercase_ascii
let header headers header = List.assoc_opt header headers

let guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

let is_websocket_upgrade headers =
  let upgrade = lowercase_header headers "upgrade" in
  let connection = lowercase_header headers "connection" in
    match (upgrade, connection) with
    | Some "websocket", Some "upgrade" ->
        print_endline "matched websocket connection";
        true
    | _ -> false

let build_message frames =
  List.fold_left (fun message f -> match f.Frame.data with
    | `Plain s -> s ^ message
    | `Masked (_, _) as data ->
      let `Plain unmasked_data = Frame.unmask data in
        unmasked_data ^ message)
  ""
  frames

module Make(Io : Interface'.Io.S) = struct

  module M_result = Interface'.Monad_result.Make(Io.M)(struct type t = string let of_exn = Printexc.to_string end)
  open Io.M
  module F = Frame.Make(Io)(M_result)

  let upgrade headers =
    let error_headers = [("sec-websocket-version","13")] in
      match is_websocket_upgrade headers with
      | true ->
        let version = lowercase_header headers "sec-websocket-version" in
        let key = header headers "sec-websocket-key" in
        let host = lowercase_header headers "host" in (* host header required but not used *)
          (match (version, host, key) with
            | Some "13", Some _, Some k ->
                let accept_key = k ^ guid |> Sha1.string |> Sha1.to_bin |> B64.encode in
                  Ok
                  [ ("upgrade", "websocket")
                  ; ("connection", "Upgrade")
                  ; ("sec-websocket-accept", accept_key)
                  ]
            | _ -> Error error_headers)
      | false -> Error error_headers

  let handle ~is_server handler ic oc =
    let state = ref Open in
    let should_mask = not is_server in
    let writer = function
      | None ->
        let f = Frame.close ~mask:should_mask 1000 in
          state := Closed;
          F.write_frame oc f
      | Some msg ->
      let frame = Frame.of_string ~mask:should_mask Text msg in
      F.write_frame oc frame in
    handler writer >>= fun handle_in ->
      let rec reader buffer =
        let handle_data_frame frame =
          let buffer' = frame::buffer in
            if frame.Frame.fin then
              let message = build_message buffer' in
                handle_in (Some message) >> lazy (reader [])
              else
                reader buffer' in
        F.read_frame ic
        >>= (function
          | Error _ -> handle_in None >> lazy (return ())
          | Ok frame -> (match frame.opcode with
            | Text | Binary -> (* text and binary handled the same, as sequence of bytes/chars *)
              if List.length buffer > 0 then
                let f = Frame.close ~mask:should_mask 1002 in
                  F.write_frame oc f
              else
                handle_data_frame frame
            | Continuation ->
              if List.length buffer = 0 then
                let f = Frame.close ~mask:should_mask 1002 in
                  F.write_frame oc f
              else
                handle_data_frame frame
            | Ping ->
                let send_pong s =
                  let f = Frame.of_string ~mask:should_mask Pong s in
                    F.write_frame oc f in
                  (match frame.data with
                    | `Masked _ as data ->
                      let `Plain s = Frame.unmask data in
                        send_pong s
                    | `Plain s ->
                      send_pong s)
            | Close ->
              (match !state with
              | Open ->
                let f = Frame.close ~mask:should_mask 1000 in
                  F.write_frame oc f
                  (* assuming closing in channel is enough... *)
                  >> lazy (if is_server then Io.close_in ic else return ())
                  >> lazy (return ())
              | Closed ->
                  if is_server then Io.close_in ic else return ())
            | _ -> reader buffer)) in
        reader []

  let handle_server h ic oc = handle ~is_server:true h ic oc

  let handle_client h ic oc = handle ~is_server:false h ic oc
end
