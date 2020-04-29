type t = {
  max_pool_size: int;
  table:
    ( string,
      ( Cohttp.Request.t * Cohttp_lwt.Body.t,
        Cohttp.Response.t * Cohttp_lwt.Body.t )
      Lwt_streams_queue.t
      Queue.t )
    Hashtbl.t;
  table_lock: Lwt_mutex.t;
}

let create ?(max_pool_size = 16) () =
  { max_pool_size; table = Hashtbl.create 16; table_lock = Lwt_mutex.create () }

let rec request session meth ?params ?data ?headers ?auth
    ?(follow_redirects = true) url =
  let { max_pool_size; table; table_lock } = session in
  let ({ Request_utils.body; uri; request_headers; _ } as request_data) =
    Request_utils.make_request_data meth ?data ?params ?headers ?auth url
  in
  let%lwt req = Request_utils.request_of_request_data request_data in
  let host_string = Uri.host uri |> Option.get in

  let%lwt () = Lwt_mutex.lock table_lock in
  let%lwt pool =
    match Hashtbl.find_opt table host_string with
    | Some pool -> pool |> Lwt.return
    | None ->
        let pool = Queue.create () in
        Hashtbl.add table host_string pool;
        pool |> Lwt.return
  in
  let%lwt handler =
    match Queue.length pool < max_pool_size with
    | true ->
        let requests_stream, push_request = Lwt_stream.create () in
        let%lwt responses_stream =
          let base_uri =
            Uri.(with_uri ~host:(host uri) ~scheme:(scheme uri) empty)
          in
          Cohttp_lwt_unix.Client.callv base_uri requests_stream
        in
        let handler = Lwt_streams_queue.create push_request responses_stream in
        handler |> Lwt.return
    | false -> Queue.pop pool |> Lwt.return
  in
  let () = Lwt_mutex.unlock table_lock in
  try%lwt
    let%lwt response, response_body =
      Lwt_streams_queue.process handler (req, Option.value body ~default:`Empty)
    in
    let result =
      Response_utils.process (response, response_body) (request session) meth
        ~uri ?params ?data ?headers ?auth ~request_headers ~follow_redirects
    in
    (* If an exception occured in [Lwt_streams_queue.process]
       (because of a closed connection) this won't be called,
       so the handler won't be reused. *)
    Queue.push handler pool;
    result
  with Failure _msg ->
    Lwt_streams_queue.close handler;
    request session meth ?params ?data ?headers ?auth ~follow_redirects url

let get session ?params ?data ?headers ?auth =
  request session `GET ?params ?data ?headers ?auth

let post session ?params ?data ?headers ?auth =
  request session `POST ?params ?data ?headers ?auth

let put session ?params ?data ?headers ?auth =
  request session `PUT ?params ?data ?headers ?auth

let delete session ?params ?data ?headers ?auth =
  request session `DELETE ?params ?data ?headers ?auth

let max_pool_size { max_pool_size; _ } = max_pool_size

let reset { table; table_lock; _ } =
  let%lwt () = Lwt_mutex.lock table_lock in
  Hashtbl.iter
    (fun _key queue -> Queue.iter Lwt_streams_queue.close queue)
    table;
  Hashtbl.reset table;
  Lwt_mutex.unlock table_lock |> Lwt.return

let close = reset
