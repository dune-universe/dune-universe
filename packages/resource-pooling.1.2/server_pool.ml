(**
This module is built around [Resource_pool]. While a pool of type
[Resource_pool.t] manages a number of resources, here we manage a cluster of
such pools. A typical use case would be a cluster of servers, where for each
server we maintain a number of connections. A user of this module can call [use]
to access one of the connections, which are served in a round-robin fashion.
*)

[@@@ocaml.warning "+A-9-44-48"]

let (>>=) = Lwt.(>>=)

let section = Lwt_log.Section.make "server-pool"
let () = Lwt_log.Section.set_level section Lwt_log.Info


module type CONF = sig
  type connection
  type server
  type serverid
  val serverid_to_string : serverid -> string
  val connect : server -> connection Lwt.t
  val close : connection -> unit Lwt.t
  val check_delay : float
  val check_server : serverid -> server -> bool Lwt.t
end

module Make (Conf : CONF) = struct

  let show = Conf.serverid_to_string

  type server_status = {
    serverid : Conf.serverid;
    desired : int;
    current : int;
    essential : bool;
    suspended : bool;
    check_server : unit -> bool Lwt.t;
    connections : Conf.connection Resource_pool.t;
  }

  let mk_server_status
        ~serverid ~desired ~essential ~check_server ~connections =
    {serverid = serverid; desired; current = 0; essential; suspended = false;
     check_server; connections}

  let servers : (Conf.serverid, server_status) Hashtbl.t = Hashtbl.create 9

  let get_status serverid = Hashtbl.find_opt servers serverid

  let non_essential_active_connection_pools () =
    let accum serverid {essential; suspended; connections} acc =
      if not essential && not suspended
        then (serverid, connections) :: acc
        else acc
    in
    Hashtbl.fold accum servers []

  let remove serverid =
    Lwt_log.ign_notice_f ~section "removing server %s" (show serverid);
    Hashtbl.remove servers serverid

  let update_current_count serverid f =
    match get_status serverid with None -> () | Some status ->
    let status = {status with current = f status.current} in
    Hashtbl.replace servers serverid status;
    Lwt_log.ign_debug_f ~section "current number of instances for %s: %d/%d"
                                 (show serverid) status.current status.desired

  (* Each server holds its own connection_pool, so a server pool is a pool of
     connection pools. HOWEVER, [server_pool] will not contain one
     connection pool per server, but [n] times the same connection pool per
     server, where [n] is the (maximum) size of the servers connection pool. *)
  let server_pool : server_status Resource_pool.t =
    let nil () = failwith "Bs_db.server_pool: invalid connection attempt" in
    (* We supply [0] as the first argument to [Resource_pool.create] as it will
       prevent [Resource_pool] to ever create a new resource on its own. This is what
       we want since new servers are to be added by the user of this module. *)
    let n = 0
    and dispose {serverid} =
      update_current_count serverid pred;
      Lwt.return_unit
    and check _ {serverid} =
      match get_status serverid with
      | None -> Lwt.return_false (* remove retired server from pool *)
      | Some _ -> Lwt.return_true
      (* | Some status -> f status.essential *)
      (* For now, do not dispose of servers upon Resource_invalid {safe =
         true} as there is currently no mechanism for reinstating them.
         Potentially it might be advisable to dispose of them temporarily. *)
    in Resource_pool.create ~check ~dispose n nil

  let server_exists serverid = Hashtbl.mem servers serverid

  let add_many
      ?(essential = false)
      ?(connect_immediately = false) ~num_conn new_servers =
    let mk_connection_pool (serverid, server) : server_status =
      Lwt_log.ign_notice_f ~section "adding server: %s" (show serverid);
      let connect () =
        Lwt_log.ign_info_f ~section "opening connection to %s" (show serverid);
        Conf.connect server >>= fun conn ->
        Lwt.return conn
      in
      let dispose conn =
        (* TODO: reopen closed connections if connect_immediately is true ? *)
        Lwt_log.ign_info_f ~section "closing connection to %s" (show serverid);
        Lwt.catch (fun () -> Conf.close conn) (fun _ -> Lwt.return_unit)
      in
      let check_server () = Conf.check_server serverid server in
      let check _ _ = Lwt.return_true in (* never close connections *)
      let connections = Resource_pool.create num_conn ~check ~dispose connect in
      let status =
        mk_server_status
          ~serverid ~desired:num_conn ~essential ~check_server ~connections
      in
      Hashtbl.add servers serverid status;
      if connect_immediately then
        for _ = 1 to num_conn do
          Lwt.async @@ fun () ->
            connect () >>= fun c ->
            try Resource_pool.add connections c; Lwt.return_unit
            with Resource_pool.Resource_limit_exceeded -> dispose c
        done;
      status
    in
    let pools = List.map mk_connection_pool @@
      List.filter (fun l -> not @@ server_exists @@ fst l) new_servers in
    for _ = 1 to num_conn do
      pools |> List.iter @@ fun conn_pool ->
        Resource_pool.add ~omit_max_check:true server_pool conn_pool;
        update_current_count conn_pool.serverid succ
    done

  let add_one ?essential ?connect_immediately ~num_conn serverid server =
    add_many ?essential ?connect_immediately ~num_conn [(serverid, server)]

  let add_existing
      ?(essential = false) ?(check_server = fun () -> Lwt.return_true)
      ~num_conn serverid connections =
    Lwt_log.ign_notice_f ~section "adding existing server: %s" (show serverid);
    let status =
      mk_server_status
        ~serverid ~desired:num_conn ~essential ~check_server ~connections
    in
    Hashtbl.add servers serverid status;
    for _ = 1 to num_conn do
      Resource_pool.add ~omit_max_check:true server_pool status;
      update_current_count serverid succ
    done

  let reactivate_server ~check_server connection_pool =
    let serverid = connection_pool.serverid in
    let check () =
      Lwt_unix.sleep Conf.check_delay >>= fun () ->
      Lwt_log.ign_debug_f ~section "checking server health of %s" (show serverid);
      Lwt.catch
        check_server
        (fun e ->
           Lwt_log.ign_info_f ~section
             "exception during health check of %s: %s"
             (show serverid) (Printexc.to_string e);
           Lwt.return_false)
    in
    let reactivate () =
      match get_status serverid with None -> Lwt.return_unit | Some status ->
      Lwt_log.ign_notice_f ~section
        "reactivating healthy server %s" (show serverid);
      Hashtbl.replace servers serverid {status with suspended = false};
      for _ = status.current to status.desired - 1 do
        Resource_pool.add ~omit_max_check:true server_pool connection_pool;
        update_current_count serverid succ
      done;
      Lwt.return_unit
    in
    let rec loop () =
      if not @@ server_exists serverid then Lwt.return_unit else
      check () >>= fun healthy -> if healthy then reactivate () else loop ()
    in loop ()

  let suspend_server ~check_server connection_pool =
    let serverid = connection_pool.serverid in
    match get_status serverid with None -> () | Some status ->
    if status.essential || status.suspended then () else begin
      Lwt_log.ign_warning_f ~section "suspending %s" (show serverid);
      Hashtbl.replace servers serverid {status with suspended = true};
      Lwt.async @@ fun () ->
        Resource_pool.clear connection_pool.connections >>= fun () ->
        reactivate_server ~check_server connection_pool
    end

  let use ?usage_attempts f =
    (* We use retry here, since elements cannot be removed from an
       [Resource_pool.t] directly. Therefore we detect whether a server has been
       removed by our own means and try again with another server it this was
       the case. Once a server has been removed (by the use of [remove]) there
       will be [n] such retries before all traces of a server have been
       erased, where [n] equals the value used for [num_conn] when the server
       was added. *)
    Resource_pool.use ~usage_attempts:9 server_pool @@ fun connection_pool ->
      let {serverid; connections} = connection_pool in
      match get_status serverid with
      | None ->
          Lwt_log.info_f ~section "cannot use %s (removed)" (show serverid)
          >>= fun () ->
          Lwt.fail Resource_pool.(Resource_invalid {safe = true})
      | Some {suspended = true} ->
          Lwt_log.info_f ~section "not using %s (suspended)" (show serverid)
          >>= fun () ->
          Lwt.fail Resource_pool.(Resource_invalid {safe = true})
      | Some {check_server} ->
        Lwt_log.debug_f ~section "using connection to %s" (show serverid)
        >>= fun () ->
        Lwt.catch
          (fun () -> Resource_pool.use ?usage_attempts connections f)
          (fun e -> match e with
             | Resource_pool.(Resource_invalid {safe = true}) ->
                 Lwt_log.warning
                   "connection unusable (safe to retry using another server)"
                 >>= fun () ->
                 Lwt.fail e
             | Resource_pool.(Resource_invalid {safe = false}) ->
                 Lwt_log.warning
                   "connection unusable (unsafe to retry using another server)"
                 >>= fun () ->
                 suspend_server ~check_server connection_pool;
                 Lwt.fail e
             | e -> Lwt.fail e
          )

  let server_statuses () =
    Hashtbl.fold (fun _ status l -> status :: l) servers []
  let servers () =
    Hashtbl.fold (fun server _ l -> server :: l) servers []

end
