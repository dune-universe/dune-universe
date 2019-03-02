open Core
open Async
module Stats = Unix.Stats

type file_info = string * Unix.Stats.t

module Which_file = struct
  type t = {
    dev : int;
    ino : int;
  } [@@deriving fields, compare]
  let of_stats { Stats.dev; ino; _ } = { dev; ino; }
end

type path = string list

type context = {
  depth : int;
  dir_name : path;
  seen : Which_file.t list;
}

let path_append path x = x :: path
let path_to_string ?base path =
  match (base, path) with
  | None,      [] -> "."
  | Some base, [] -> base
  | None,      _  ->         String.concat ~sep:"/" (List.rev path)
  | Some base, _  -> base ^/ String.concat ~sep:"/" (List.rev path)

module Options = struct
  type error_handler =
    | Ignore
    | Print
    | Raise
    | Handle_with of (string -> unit Deferred.t)

  type t = {
    min_depth: int;
    max_depth: int option;
    follow_links: bool;
    on_open_errors: error_handler;
    on_stat_errors: error_handler;
    filter: (file_info -> bool Deferred.t) option;
    skip_dir: (file_info -> bool Deferred.t) option;
    relative_paths : bool;
  }

  let default = {
    min_depth = 1;
    max_depth = None;
    follow_links = false;
    on_open_errors = Raise;
    on_stat_errors = Raise;
    filter = None;
    skip_dir = None;
    relative_paths = false;
  }

  let ignore_errors = {
    default with
    on_open_errors = Ignore;
    on_stat_errors = Ignore
  }
end
module O = Options

type t = {
  base: string;
  options: Options.t;
  mutable current_context: context;
  mutable to_visit: context list;
  mutable current_handle: [ `Just_created | `Starting | `Handle of Unix.dir_handle ];
  mutable closed: bool;
}

let full_path_name   t path =
  path_to_string ~base:t.base path
let output_path_name t path =
  path_to_string ?base:(if t.options.O.relative_paths then None else Some t.base) path

let open_next_dir t =
  let i = Ivar.create () in
  let rec loop t =
    match t.to_visit with
    | [] -> Ivar.fill i None
    | context :: rest ->
      upon (Monitor.try_with ~rest:`Raise (fun () ->
        t.to_visit <- rest;
        Unix.opendir (full_path_name t context.dir_name) >>| (fun handle ->
          t.current_handle <- `Handle handle;
          t.current_context <- context;
          Some ())
      )) (function
        | Ok r -> Ivar.fill i r
        | Error e ->
          let e = Monitor.extract_exn e in
          match t.options.O.on_open_errors with
          | O.Ignore -> loop t
          | O.Raise -> raise e
          | O.Handle_with f ->
            upon (f (output_path_name t context.dir_name)) (fun () -> loop t)
          | O.Print ->
            Print.eprintf "unable to open %s - %s\n"
              (output_path_name t context.dir_name) (Exn.to_string e);
            loop t)
  in
  loop t;
  Ivar.read i
;;

let closedir t =
  match t.current_handle with
  | `Just_created | `Starting -> return ()
  | `Handle current_handle ->
    Deferred.ignore
      (Monitor.try_with ~rest:`Raise (fun () -> Unix.closedir current_handle)
       : (unit, exn) Result.t Deferred.t)
;;

let close t =
  if not t.closed then
    begin
      t.closed <- true;
      closedir t >>| fun () ->
      t.to_visit <- [];
    end
  else Deferred.unit
;;

let seen_before (context : Which_file.t list) stats =
  List.exists ~f:([%compare.equal: Which_file.t] (Which_file.of_stats stats)) context

let stat t seen path =
  let full_fn = full_path_name t path in
  let output_fn  = output_path_name t path in
  Monitor.try_with ~rest:`Raise (fun () ->
    Unix.lstat full_fn >>= function
    | { kind = `Link; _ } as lstat when t.options.O.follow_links ->
      (* Symlink. Try following it. *)
      Unix.stat full_fn
      >>| (function
        (* When a symlink points to its ancestor directory, report only the symlink. *)
        | ({ kind = `Directory; _ } as stat) when seen_before seen stat -> lstat
        | stat -> stat)
    | stat -> return stat
  ) >>= (function
    | Ok stat -> return (Some (output_fn, path, stat))
    | Error e ->
      let e = Monitor.extract_exn e in
      match t.options.O.on_stat_errors with
      | O.Ignore -> return None
      | O.Raise -> raise e
      | O.Handle_with f -> f output_fn >>| (fun () -> None)
      | O.Print ->
        Print.eprintf "unable to stat %s - %s\n" output_fn (Exn.to_string e);
        return None)
;;

let handle_dirs t (output_fn, path, stats) =
  let info = output_fn, stats in
  let visit () =
    t.to_visit <- {
      dir_name = path;
      seen = Which_file.of_stats stats :: t.current_context.seen;
      depth = t.current_context.depth + 1; }
      :: t.to_visit;
    return (Some info)
  in
  let maybe_visit () =
    match t.options.O.skip_dir with
    | None   -> visit ()
    | Some f ->
      f info
      >>= fun skip ->
      if skip then return None else visit ()
  in
  let maybe_return_info () =
    match t.options.O.skip_dir with
    | None   -> return (Some info)
    | Some f ->
      f info
      >>= fun skip ->
      if skip then return None else return (Some info)
  in
  match stats.Stats.kind with
  | `Directory ->
    begin match t.options.O.max_depth with
    | None           -> maybe_visit ()
    | Some max_depth ->
      if t.current_context.depth < max_depth
      then maybe_visit ()
      else maybe_return_info ()
    end
  | _ ->
    return (Some info)
;;

let filter t file =
  match file with
  | None      -> return None
  | Some file ->
    if t.current_context.depth < t.options.O.min_depth then
      return None
    else match t.options.O.filter with
      | None   -> return (Some file)
      | Some f -> f file >>| (fun keep -> if keep then Some file else None)
;;

exception Attempt_to_use_closed_find of [`Most_recent_dir of string] [@@deriving sexp] ;;

let ensure_not_closed t = if t.closed then
    raise (Attempt_to_use_closed_find
             (`Most_recent_dir (output_path_name t t.current_context.dir_name))) ;;

(* returns the next file from the conceptual stream and updates the state of t - this
   is the only way that t should ever be updated *)
let next t =
  ensure_not_closed t;
  let i = Ivar.create () in
  let handle_child path =
    (* each function in this bind returns None if the file should be skipped, and
       Some f i if it thinks it's ok to emit - possibly updating the state or
       transforming f along the way *)
    let (>>>=)
      : type v w. v option Deferred.t -> (v -> w option Deferred.t) -> w option Deferred.t
      = fun v f ->
        v
        >>= function
        | None   -> return None
        | Some v -> f v
    in
    stat t t.current_context.seen path
    >>>= handle_dirs t
    >>= filter t
  in
  let with_next_dir k =
    upon (open_next_dir t) (function
      | None -> upon (close t) (fun () -> Ivar.fill i None)
      | Some () -> k ())
  in
  let rec loop () =
    let handle_child_or_loop path =
      handle_child path
      >>> function
      | None -> loop ()
      | r    -> Ivar.fill i r
    in
    match t.current_handle with
    | `Just_created ->
      begin match t.options.O.max_depth with
      | Some d when d < 0 -> upon (close t) (fun () -> Ivar.fill i None)
      | None | Some _ ->
        t.current_handle <- `Starting;
        handle_child_or_loop t.current_context.dir_name
      end
    | `Starting ->
      with_next_dir loop
    | `Handle current_handle ->
      upon (Monitor.try_with ~rest:`Raise (fun () ->
        Unix.readdir_opt current_handle)
      ) (function
      | Ok (Some ("." | "..")) -> loop ()
      | Ok (Some basename) ->
        handle_child_or_loop (path_append t.current_context.dir_name basename)
      | Ok None -> upon (closedir t) (fun () -> with_next_dir loop)
      | Error e ->
        upon (closedir t) (fun () -> raise e))
  in
  loop ();
  Ivar.read i
;;

let create ?(options=Options.default) dir =
  {
    base = dir;
    options = options;
    to_visit = [];
    current_context = {
      dir_name = [];
      seen = [];
      depth = 0;
    };
    current_handle = `Just_created;
    closed = false;
  }
;;

let fold t ~init ~f =
  Deferred.create (fun i ->
    let rec loop acc =
      upon (next t) (function
        | None -> Ivar.fill i acc
        | Some file -> upon (f acc file) loop)
    in
    loop init)
;;

let iter t ~f = fold t ~init:() ~f:(fun () file -> f file)

let to_list t =
  (fold t ~init:[] ~f:(fun acc file -> return (file :: acc))) >>| List.rev
;;

let find_all ?options dir =
  to_list (create ?options dir)
;;
