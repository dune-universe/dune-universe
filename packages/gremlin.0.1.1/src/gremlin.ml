module Websocket = struct
  let new_connection uri =
    let%lwt endp = Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system in
    let%lwt client = Conduit_lwt_unix.(endp_to_client ~ctx:default_ctx endp) in
    let%lwt recv, send = Websocket_lwt_unix.with_connection ~ctx:Conduit_lwt_unix.default_ctx client uri in
    Lwt.return (recv, send)

  type op_processor =
    | Session of string
    | Standard

  type message_status =
    | Response_invalid of response_invalid
    | Request_failed of request_failed
    | Unmatched_request_id
    | Good of Yojson.Basic.t

  and request_failed =
    | Non_200_status_code of Yojson.Basic.t

  and response_invalid =
    | No_status_field
    | Missing_fields
    | Missing_status_fields
    | Missing_field of string
    | Invalid_status_json_type of Yojson.Basic.t
    | Invalid_result_json_type of Yojson.Basic.t
    | Invalid_result_field
    | Invalid_request_id_json_type of Yojson.Basic.t
    | Result_fields_wrong
    | Json_parse_failure
    | Unknown_json_parse_failure

  let check_result = function
    | Good m as message ->
      begin try
        let result = Yojson.Basic.Util.member "result" m in
        if (CCList.equal
             (String.equal)
             (CCList.sort (String.compare) (Yojson.Basic.Util.keys result))
             (CCList.sort (String.compare) ["data";"meta"])
        ) then
          message
        else (
          List.iter (Printf.printf "Result key: %s\n%!") (Yojson.Basic.Util.keys result);
          Response_invalid Result_fields_wrong
        )
      with
        | _ -> Response_invalid (Missing_field "result")
        end
    | _ as status -> status

  let check_fields = function
    | Good m as message ->
      if (CCList.equal
           (String.equal)
           (Yojson.Basic.Util.keys m)
           ["requestId";"status";"result"]
      ) then
        message
      else (
        List.iter (Printf.printf "Field key: %s\n%!") (Yojson.Basic.Util.keys m);
        Response_invalid Missing_fields
      )
    | _ as status -> status

  let check_status = function
    | Good m as message ->
      begin match Yojson.Basic.Util.member "status" m with
        | `Null -> Response_invalid (Missing_field "status")
        | _ as status ->
          if (CCList.equal
               (String.equal)
               (CCList.sort (String.compare) (Yojson.Basic.Util.keys status))
               (CCList.sort (String.compare) ["code";"attributes";"message"])
          ) then (
            begin match
                Yojson.Basic.Util.member "code" status
            with
            | `Int i ->
              begin match i with
                | 200 | 204  -> message
                | _ -> Request_failed (Non_200_status_code (Yojson.Basic.Util.member "status" m))
              end
            | json -> Response_invalid (Invalid_status_json_type json)
            end
          ) else (
            Response_invalid Missing_status_fields
          )
      end
    | _ as status -> status

  let check_request_id request_id = function
    | Good m as message ->
      begin match Yojson.Basic.Util.member "requestId" m with
        | `String s ->
          if String.equal s request_id then
            message
          else
            Unmatched_request_id
      | json -> Response_invalid (Invalid_request_id_json_type json)
      end
    | _ as status -> status

  let create_gremlin_query_request_response op_processor query =
    let request_id = Uuid_unix.create () |> Uuid.to_string in
    let open Yojson.Basic in

    let request_payload : Yojson.Basic.t =
      let request_id = ("requestId", `String (request_id)) in
      let op = ("op", `String "eval") in
      let processor = match op_processor with
      | Standard -> ("processor", `String "")
      | Session _ -> ("processor", `String "session")
      in
      let args =
        [
          ("gremlin", `String query);
          ("language", `String "gremlin-groovy");
        ]
      in
      let args = match op_processor with
      | Session s -> ("session", `String s) :: args
      | Standard -> args
      in
      `Assoc [
        request_id;
        op;
        processor;
        ( "args", `Assoc args );
      ]
    in

    let response_handler msg  =
      let message =
        try Good (Yojson.Basic.from_string msg) with
        | Finally _ -> Response_invalid Json_parse_failure
        | _ -> Response_invalid Unknown_json_parse_failure
      in
      let message = check_fields message in
      let message = check_request_id request_id message in
      let message = check_result message in
      let message = check_status message in
      message
    in
    (request_payload, response_handler)

  let print_message_status = function
    | Response_invalid r -> begin match r with
      | No_status_field -> Printf.printf "ERROR: Response_invalid: No_status_field%!"
      | Missing_status_fields -> Printf.printf "ERROR: Response_invalid: Missing_status_fields%!"
      | Missing_fields -> Printf.printf "ERROR: Response_invalid: Missing_fields%!"
      | Missing_field s -> Printf.printf "ERROR: Response_invalid: Missing_field %s\n%!" s
      | Invalid_status_json_type j -> Printf.printf "ERROR: Response_invalid: Invalid_status_json_type %s\n%!" (Yojson.Basic.pretty_to_string j)
      | Invalid_result_json_type j -> Printf.printf "ERROR: Response_invalid: Invalid_result_json_type %s\n%!" (Yojson.Basic.pretty_to_string j)
      | Invalid_result_field -> Printf.printf "ERROR: Response_invalid: Invalid_result_field%!"
      | Invalid_request_id_json_type j ->  Printf.printf "ERROR: Response_invalid: Invalid_result_json_type %s\n%!" (Yojson.Basic.pretty_to_string j)
      | Result_fields_wrong -> Printf.printf "ERROR: Response_invalid: Result_fields_wrong%!"
      | Json_parse_failure -> Printf.printf "ERROR: Response_invalid: Json_parse_failure%!"
      | Unknown_json_parse_failure -> Printf.printf "ERROR: Response_invalid: Unknown_json_parse_failure%!"
    end
    | Request_failed r -> begin match r with
      | Non_200_status_code j -> Printf.printf "ERROR: Request_failed: Non_200_status_code\n%s\n%!" (Yojson.Basic.pretty_to_string j)
      end
    | Unmatched_request_id -> Printf.printf "ERROR: Unmatched_request_id%!"
    | Good j -> Printf.printf "SUCCESS\n%s\n%!" (Yojson.Basic.pretty_to_string j)

  let rec stream_receiver (stream : Websocket.Frame.t Lwt_stream.t) f send =
    let open Websocket.Frame in
    match%lwt Lwt_stream.peek stream with
    | None ->
      let%lwt () = Lwt_io.printf "No frame from peek\n%!" in
      stream_receiver stream f send
    | Some fr ->
      let%lwt () = Lwt_io.printf "Got frame from peek\n%!" in
      begin match fr.opcode with
        | Opcode.Text
        | Opcode.Binary ->
          let%lwt () = Lwt_io.printf "Binary or text from peek\n%!" in
          begin match f fr.content with
            | Good _
            | Response_invalid _
            | Request_failed _ ->
              begin match%lwt Lwt_stream.get stream with
                | Some fr ->
                let%lwt () = Lwt_io.printf "Got frame from get\n%!" in
                  begin match fr.opcode with
                    | Opcode.Text
                    | Opcode.Binary ->
                      let%lwt () = Lwt_io.printf "Binary or text from get\n%!" in
                      begin match f fr.content with
                        | Good j ->
                          let%lwt () = Lwt_io.printf "Processed frame\n%s\n%!" (Yojson.Basic.pretty_to_string j) in
                          Lwt.return (Ok j)
                        | _ as e -> Lwt.return (Error e)
                      end
                    | _ -> stream_receiver stream f send
                  end
                | None ->
                  let%lwt () = Lwt_io.printf "No frame from get\n%!" in
                  stream_receiver stream f send
              end
            |  _ -> stream_receiver stream f send
          end
        | _ -> stream_receiver stream f send
      end

  let run_query conn query =
    let%lwt recv, send = conn in
    let in_stream = Websocket_lwt_unix.mk_frame_stream recv in
    let req, resp = create_gremlin_query_request_response Standard query in
    let%lwt () = send @@ Websocket.Frame.create ~content:(req |> Yojson.Basic.to_string) () in
    stream_receiver in_stream resp send

  let run_queries_transaction conn queries =
    let%lwt recv, send = conn in
    (* generate session id (UUID) *)
    let session = Uuid_unix.create () |> Uuid.to_string in
    (* Create request payload and response handler *)
    let in_stream = Websocket_lwt_unix.mk_frame_stream recv in

    let execute_queries () =
      let reqs_and_handlers =
        List.map
          (create_gremlin_query_request_response (Session session))
          queries
      in

      Lwt_list.map_s
      (fun (req, resp) ->
        let req_s = req |> Yojson.Basic.to_string in
        let%lwt () = Lwt_io.printf "Sending request\n%s\n%!" req_s in
        let%lwt () = send @@ Websocket.Frame.create ~content:req_s () in
        let%lwt () = Lwt_io.printf "Reading response from stream\n%!" in
        stream_receiver in_stream resp send
      )
      reqs_and_handlers
    in

    let tx_commit = "g.tx().commit()" in
    let commit_transaction () =
      let%lwt () = Lwt_io.printf "Committing transaction\n%!" in
      let req, resp = create_gremlin_query_request_response (Session session) tx_commit in
      let%lwt () = send @@ Websocket.Frame.create ~content:(req |> Yojson.Basic.to_string) () in
      stream_receiver in_stream resp send
    in

    let%lwt e = execute_queries () in
    let first_failure =
        List.find_opt
          (fun i ->
            CCResult.is_error i
          )
          e
    in

    match first_failure with
    | Some e ->
      let%lwt () = Lwt_io.printf "At least one query failed.\n%!" in
      Lwt.return e
    | None ->
      begin match%lwt commit_transaction () with
        | Ok _ as ok ->
          let%lwt () = Lwt_io.printf "Transaction succeeded\n%!" in
          Lwt.return ok
        | Error _ as e ->
          let%lwt () = Lwt_io.printf "Transaction commit failed\n%!" in
          Lwt.return e
      end

end
