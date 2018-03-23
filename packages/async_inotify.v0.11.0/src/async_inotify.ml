open Core
(* we want to be very specific about which Unix methods we use in this module *)
module Base_unix = Unix
open Async
module Async_unix = Unix
module Stats = Async_unix.Stats
module Unix = struct end
module Inotify = Ocaml_inotify.Inotify
module Find = Async_find
module Fopts = Find.Options

module Event = struct
  type move =
    | Away of string
    | Into of string
    | Move of string * string
  [@@deriving sexp_of]

  type t =
    | Created of string
    | Unlinked of string
    | Modified of string
    | Moved of move
    | Queue_overflow
  [@@deriving sexp_of]

  let move_to_string m =
    match m with
    | Away s -> sprintf "%s -> Unknown" s
    | Into s -> sprintf "Unknown -> %s" s
    | Move (f, t) -> sprintf "%s -> %s" f t
  ;;

  let to_string t =
    match t with
    | Created s -> sprintf "created %s" s
    | Unlinked s -> sprintf "unlinked %s" s
    | Moved mv -> sprintf "moved %s" (move_to_string mv)
    | Modified s -> sprintf "modified %s" s
    | Queue_overflow -> "queue overflow"
  ;;
end
open Event

type t = {
  fd: Base_unix.File_descr.t;
  watch_table: (Inotify.watch, string) Hashtbl.t;
  path_table: Inotify.watch String.Table.t;
  tail: Event.t Tail.t;
  select_events : Inotify.selector list;
}
type file_info = string * Async_unix.Stats.t

type modify_event_selector = [ `Any_change | `Closed_writable_fd ]

let (^/) = Filename.concat

(** [add t path] add the path to t to be watched *)
let add t path =
  In_thread.run (fun () ->
      let watch = Inotify.add_watch t.fd path t.select_events in
      Hashtbl.set t.watch_table ~key:watch ~data:path;
      Hashtbl.set t.path_table ~key:path ~data:watch;
    )
;;

(* adds all the directories under path (including path) to t *)
let add_all ?skip_dir t path =
  let options = {Fopts.default with
      Fopts.on_open_errors = Fopts.Print;
      on_stat_errors = Fopts.Print;
      skip_dir
    }
  in
  add t path >>= fun () ->
  let f = Find.create ~options path in
  Find.fold f ~init:[] ~f:(fun files (fn,stat) ->
    if stat.Stats.kind = `Directory then add t fn >>| (fun () -> (fn,stat) :: files)
    else return ((fn,stat) :: files))
;;

(** [remove t path] remove the path from t *)
let remove t path =
  In_thread.run (fun () ->
      Option.iter (Hashtbl.find t.path_table path) ~f:(fun watch ->
          Inotify.rm_watch t.fd watch;
          Hashtbl.remove t.watch_table watch;
          Hashtbl.remove t.path_table path;
    )
  )
;;

let build_raw_stream fd watch_table =
  let tail = Tail.create () in
  don't_wait_for (In_thread.run (fun () ->
      while true do
        let (_ :Base_unix.Select_fds.t) =
          Base_unix.select
            ~restart:true ~read:[fd] ~write:[] ~except:[] ~timeout:`Never ()
        in
        let events = Inotify.read fd in
        Thread_safe.run_in_async_exn (fun () ->
            let ev_kinds = List.filter_map events ~f:(fun (watch, ev_kinds, trans_id, fn) ->
              if Inotify.int_of_watch watch = -1 (* queue overflow event is always reported on watch -1 *) then
                let maybe_overflow =
                  List.filter_map ev_kinds ~f:(fun ev ->
                    match ev with
                    | Inotify.Q_overflow -> Some (ev, trans_id, "<overflow>")
                    | _ -> None
                  )
                in
                if maybe_overflow = [] then None else Some maybe_overflow
              else
                match Hashtbl.find watch_table watch with
                | None ->
                    Print.eprintf "Events for an unknown watch (%d) [%s]"
                      (Inotify.int_of_watch watch)
                      (String.concat ~sep:", "
                        (List.map ev_kinds ~f:Inotify.string_of_event_kind));
                    None
                | Some path ->
                    let fn = match fn with None -> path | Some fn -> path ^/ fn in
                    Some (List.map ev_kinds ~f:(fun ev -> (ev, trans_id, fn)))
              ) |> List.concat
            in
            let pending_mv,actions =
              List.fold ev_kinds ~init:(None,[])
                ~f:(fun (pending_mv,actions) (kind, trans_id, fn) ->
                  let add_pending lst =
                    match pending_mv with
                    | None -> lst
                    | Some (_,fn) -> Moved (Away fn) :: lst
                  in
                  match kind with
                  | Inotify.Moved_from -> (Some (trans_id, fn), add_pending actions)
                  | Inotify.Moved_to ->
                      begin
                        match pending_mv with
                        | None ->
                            (None, (Moved (Into fn)) :: actions)
                        | Some (m_trans_id,m_fn) ->
                            if m_trans_id = trans_id then
                              (None,
                                (Moved (Move (m_fn, fn))) :: actions)
                            else
                              (None,
                                (Moved (Away m_fn)) ::
                                (Moved (Into fn)) :: actions)
                      end
                  | Inotify.Move_self ->
                      (Some (trans_id, fn)), add_pending actions
                  | Inotify.Create ->
                      None, (Created fn) :: add_pending actions
                  | Inotify.Delete ->
                      None, (Unlinked fn) :: add_pending actions
                  | Inotify.Modify | Inotify.Close_write ->
                      None, (Modified fn) :: add_pending actions
                  | Inotify.Q_overflow ->
                      None, Queue_overflow :: add_pending actions
                  | Inotify.Delete_self -> None, add_pending actions
                  | Inotify.Access | Inotify.Attrib
                  | Inotify.Open   | Inotify.Ignored
                  | Inotify.Isdir  | Inotify.Unmount
                  | Inotify.Close_nowrite -> (None, add_pending actions)
                )
            in
            let actions = List.rev
              (match pending_mv with
              | None -> actions
              | Some (_,fn) -> Moved (Away fn) :: actions)
            in
            List.iter actions ~f:(Tail.extend tail)
          )
      done
    ));
  tail
;;

let build_tail ~watch_new_dirs t =
  let raw_tail = build_raw_stream t.fd t.watch_table in
  don't_wait_for (Stream.iter' (Tail.collect raw_tail) ~f:(fun ev ->
    if not watch_new_dirs then return (Tail.extend t.tail ev)
    else
      match ev with
      | Queue_overflow
      | Unlinked _ | Moved _ | Modified _ -> return (Tail.extend t.tail ev);
      | Created path ->
        Monitor.try_with (fun () -> Async_unix.stat path) >>= function
        | Error _ -> (* created file has already disappeared *) return ()
        | Ok stat ->
          match stat.Async_unix.Stats.kind with
          | `File | `Char | `Block | `Link | `Fifo | `Socket ->
            return (Tail.extend t.tail (Created path));
          | `Directory ->
            Tail.extend t.tail (Created path);
            add_all t path >>| fun _ -> ()
  ))
;;

let create_with_unbuilt_tail ~modify_event_selector =
  In_thread.run Inotify.create
  >>= fun fd ->
  In_thread.run (fun () -> Base_unix.set_close_on_exec fd)
  >>| fun () ->
  let watch_table = Hashtbl.Poly.create () ~size:10 in
  let modify_selector : Inotify.selector =
    match modify_event_selector with
    | `Any_change -> S_Modify
    | `Closed_writable_fd -> S_Close_write
  in
  {
    fd;
    watch_table;
    path_table = Hashtbl.Poly.create () ~size:10;
    tail = Tail.create ();
    select_events = [
      S_Create;
      S_Delete;
      modify_selector;
      S_Move_self;
      S_Moved_from;
      S_Moved_to;
    ];
  }
;;

let create_empty ~modify_event_selector =
  let%map t = create_with_unbuilt_tail ~modify_event_selector in
  build_tail ~watch_new_dirs:false t;
  t
;;

let create ?(modify_event_selector = `Any_change) ?(recursive=true) ?(watch_new_dirs=true) path =
  (* This function used to call: [Core_extended.Filename.expand path]
     But this is the wrong place for such an expansion.
     The caller should do this if required.
     By removing this call, we avoid the dependency of this library on core_extended.
  *)
  let%bind t = create_with_unbuilt_tail ~modify_event_selector in
  let skip_dir = if recursive then None else Some (fun _ -> return true) in
  let%map initial_files = add_all ?skip_dir t path in
  build_tail ~watch_new_dirs t;
  (t, initial_files)
;;

let stop t =
  In_thread.run (fun () -> Base_unix.close t.fd)
;;

(** [stream t] returns a stream of filesystem events *)
let stream t = Tail.collect t.tail

let pipe t = Pipe.of_stream_deprecated (stream t)
