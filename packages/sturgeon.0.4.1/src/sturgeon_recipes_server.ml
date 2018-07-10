open Sturgeon

type server = {
  client: unit -> (Session.t option * (Session.t -> unit) option);

  mutable socket: Unix.file_descr option;
  mutable clients: (Unix.file_descr) list;
  connections:
    (Unix.file_descr,
     (unit -> Sexp.basic option) * Session.output * Session.status)
    Hashtbl.t;
}

let server ~client name =
  let dir = Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "sturgeon.%d" (Unix.getuid ())) in
  if not (Sys.file_exists dir) then
    Unix.mkdir dir 0o770;
  let name = Filename.concat dir
      (Printf.sprintf "%s.%d.sturgeon" name (Unix.getpid ())) in
  let socket = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let addr = Unix.ADDR_UNIX name in
  Unix.bind socket addr;
  at_exit (fun () -> Unix.unlink name);
  Unix.listen socket 3;
  { socket = Some socket; clients = []; connections = Hashtbl.create 7;
    client }

let accept server =
  match server.socket with
  | None -> ()
  | Some socket ->
    let client, _ = Unix.accept socket in
    let oc = Unix.out_channel_of_descr client in
    let olock = Mutex.create () in
    let send sexp =
      Mutex.lock olock;
      try
        Sexp.tell_sexp (output_string oc) sexp;
        output_char oc '\n';
        flush oc;
        Mutex.unlock olock;
      with exn ->
        Mutex.unlock olock;
    in
    let greetings, cogreetings = server.client () in
    let received, status = Session.connect ?greetings ?cogreetings send in
    let stdin = Sexp.of_file_descr ~on_read:ignore client in
    server.clients <- client :: server.clients;
    Hashtbl.replace server.connections
      client (stdin, received, status)

let filter_fd server fd =
  if Hashtbl.mem server.connections fd then true
  else begin
    Unix.close fd;
    false
  end

let rec main_loop server =
  match server.socket with
  | None -> ()
  | Some socket ->
    server.clients <- List.filter (filter_fd server) server.clients;
    match Unix.select (socket :: server.clients) [] [] 1.0 with
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> main_loop server
    | (r, _, _) ->
      let rec pump fd (stdin, received, status) =
        match stdin () with
        | None -> Hashtbl.remove server.connections fd
        | exception (Sys_error _) ->
          Hashtbl.remove server.connections fd
        | Some sexp ->
          begin try received sexp;
            with _ -> ()
          end;
          match Unix.select [fd] [] [] 0.0 <> ([],[],[]) with
          | true -> pump fd (stdin, received, status)
          | false -> ()
          | exception Unix.Unix_error (Unix.EINTR, _, _) -> ()
      in
      let process fd =
        if fd = socket then
          try accept server
          with _ -> ()
        else
          pump fd (Hashtbl.find server.connections fd)
      in
      List.iter process r;
      main_loop server

let main_loop ?(keep_sigpipe=false) server =
  if not keep_sigpipe then
    (try ignore (Sys.signal Sys.sigpipe Sys.Signal_ignore)
     with _ -> ());
  main_loop server

let stop_server server =
  match server.socket with
  | None -> ()
  | Some socket ->
    server.socket <- None;
    server.clients <- [];
    let clients = server.clients in
    Hashtbl.clear server.connections;
    List.iter Unix.close clients;
    Unix.close socket

let text_server name f =
  let open Sexp in
  server ~client:(fun () ->
      let greetings, shell = Stui.buffer_greetings () in
      Some greetings, Some (fun args -> f ~args shell)
    )
    name
